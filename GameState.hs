{-# LANGUAGE ScopedTypeVariables,TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module GameState
  where

import Base
import Interference
import View() -- Show instances for error messages
import qualified Data.Set as S
import qualified Data.Map as M
--import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
-- import Data.Foldable
-- import Data.Maybe (maybeToList)
import Control.Monad (foldM)
import Control.Arrow (first,(***))
-- first: apply function on fst-element in tuple; f *** g = \(a,b) -> (f a,g b)

-- import Debug.Trace

mkGSfromObs :: TotalObservations -> MayFail GameState
mkGSfromObs obs = fmap (GS obs) $ computeCHfromObs obs
  
-- creates initial gamestate from an initial set of player worlds
initGS :: S.Set PlayerWorld -> MayFail GameState
initGS pws = mkGSfromObs (M.singleton 0 (pws,S.empty))

{-
Assumptions:
. there is at least one time-step with an observation
  where a non-empty mapsize is used
-}
computeCHfromObs :: TotalObservations -> MayFail ConsHistory
computeCHfromObs obs = 
  do  
    let initCH = defaultConsHistory (maxT,mapSize)
    applyAllObservations obs initCH
  where mapSize = maybe (0,'A') (getSize . fst) $ M.minView obs
        getSize (pws,_) = case S.elems pws of 
                      [] -> (0,'A')
                      (pw:_) -> ssize pw
        
        -- the maximum time is either the time of the latest observation
        -- or the latest time referenced in a teleportation
        maxT = max (maybe (-1) (fst.fst) $ M.maxViewWithKey obs) maxTeletime
        maximum' :: Foldable t => t Int -> Int
        maximum' = foldr max (-1)
        maxTeletime :: Int
        maxTeletime = maximum' $ M.map maxPerTime obs
        maxPerTime (_,pwts) = maximum' $ S.map maxPerSpace pwts
        maxPerSpace = maximum' . M.map blockContent . sobservations
        blockContent :: BlockContentT -> Int
        blockContent = maximum' . MS.map (maxDestTimePT . snd3) . bctps
        maxDestTimePT pat =
          runpat maxDestTimePA (\_ _ -> -1) maxDestTimePA (-1) pat
        maxDestTimePA (Teleport _ _ t) = t
        maxDestTimePA _ = -1
;

{-      
1. create ConsHistory of maximal size
    size is equal to all maps size. we assume,
    that all maps are of same size.
    time goes from t=0 to the maximal time mentioned.
    This can be the maximal time observed - but
    it can also be a higher time referenced in a teleportation.
  
2. start at beginning and apply player actions one at a time

3. halt at first error (or at all errors)

computeCHfromObs is used during intermediate states of puzzle
solving to compute a minimally-assuming space-time consistent
with the current observations.
the function concreteHistory makes such a minimal space-time
concrete such that every element is uniquely defined.
-}

-- applies each observation one after a time.
-- halts at the firstcontradiction that occurred
-- if everything is consistent, then a ConsHistory is returned
applyAllObservations :: Timed (S.Set PlayerWorld, S.Set PlayerWorldT) ->
                        ConsHistory -> MayFail ConsHistory
applyAllObservations mp ch0 = foldlWithKeyM f ch0 mp
  where
    f :: Time -> (S.Set PlayerWorld, S.Set PlayerWorldT) -> ConsHistory -> MayFail ConsHistory
    f t (pws,pwts) ch = do ch2 <- foldM (applyPW  t) ch  (S.toList pws )
                           foldM (applyPWT t) ch2 (S.toList pwts)
;

applyPW :: Int -> ConsHistory -> PlayerWorld -> MayFail ConsHistory
applyPW t ch0 pw = applypwObs pw addOpenObs addClosedObs
  where
    addOpenObs = foldlWithKeyM (\pos bc -> addWhenConsistent (t,pos) bc) ch0 . sobservations
    addClosedObs x = let (pos,bc) = sobservations x in addWhenConsistent (t,pos) bc ch0
    addWhenConsistent :: TimePos -> BlockContent -> ConsHistory -> MayFail ConsHistory
    addWhenConsistent tpos bc ch = {- check blockcontent(T) player-observation for self-consistency. though this should not happen in the first place -}
         (\x -> insertCH tpos x ch) <$>
           atCH tpos (failPlayerObsOutOfBounds tpos ch)
                (maybe (success bc) (\bc' -> if bc == bc' then success bc else failPlayObsContraHistory tpos bc bc'))
                ch
  -- assumes, that ch is consistent already.
  -- it is inconcsistent, if (t,pos) is out-of-bounds
  -- it is inconcsistent, if there is already a differing Just value at (t,pos)
  -- it is consistent, if there is a Nothing at (t,pos) and
  -- the network stays consistent after adding it.
;

failPlayObsContraHistory :: TimePos -> BlockContent -> BlockContent -> MayFail a
failPlayObsContraHistory tpos bc bc' = failing $ "applyPW: players observation contradicts with established history at "++show tpos ++". observed: "++show bc ++ ", established: " ++ show bc'

failPlayerObsOutOfBounds :: TimePos -> ConsHistory -> MayFail a
failPlayerObsOutOfBounds tpos ch = failing $ "applyPW[unusual]: players observation "++show tpos++" is out-of-bounds in history. history size = "++ show (chsize ch) ++ ", with ch:\n" ++ show ch


-- TODO: consistency check can be made faster by memoizing
-- the bidirectional-dependencies. similarities to arc-consistency algo. for Constraint Solving Networks


-- returns the set of contradiction descriptions currently in the ConsHistory
contradictions :: ConsHistory -> [CondRes]
contradictions ch = {- we assume that out-of-bounds is a problem. -}
  do let (maxT,(maxX,maxY)) = chsize ch
     t <- [0..maxT]
     x <- [0..maxX]
     y <- ['A'..maxY]
     let curr = (t,(x,y))
         checkbc  = maybe []{- unknowns ok -} (\bc  -> runChecks curr bc ch)
         checkbct = maybe []{- unknowns ok -} (\bct -> runChecks curr bct ch)
     uncurry (++) $ atCHboth curr ["block(T) "++show curr++" out-of-bounds"] checkbc checkbct ch
;

{- MAIN FUNCTION: runChecks (from Block class) -}
runChecks :: Block b => TimePos -> b -> ConsHistory -> [CondRes]
runChecks curr b ch =
  let missing = findMissingIndices (ccneeds cc) ch
      cc = interferesWithBlock curr b
  in  filter isContradiction $ selfconsistent b : 
        if S.null missing
          then ccrun cc (flatten (chspace ch))
          else map (listFailReferenceOutOfBounds curr) . S.toList $ missing
;

flatten :: Timed (Space a) -> M.Map TimePos a
flatten mp = M.fromAscList (M.toAscList mp >>= \(t,mpi) -> M.toAscList mpi >>= \(pos,x) -> [((t,pos),x)])

findMissingIndices :: S.Set TimePos -> ConsHistory -> S.Set TimePos
findMissingIndices ks = (ks S.\\) . M.keysSet . flatten . chspace

listFailReferenceOutOfBounds :: TimePos -> TimePos -> CondRes
listFailReferenceOutOfBounds curr other = show curr ++ " references out-of-bounds "++show other

atCH :: TimePos -> a {- out of bounds -} -> (Maybe BlockContent -> a) -> ConsHistory -> a
atCH tpos z f = fst . atCHboth tpos z f (const (error "atCH: this cannot happen"))

-- combine two accesses into one
atCHboth :: TimePos -> a {- out of bounds -} -> (Maybe BlockContent -> a) -> (Maybe BlockContentT -> a) -> ConsHistory -> (a,a)
atCHboth (t,pos) z f g = maybe (z,z) (f *** g) . (>>= M.lookup pos) . M.lookup t . chspace

-- inserts the blockContent into the cons-history.
-- assumes, that time-pos is not out-of-bounds
insertCH :: TimePos -> BlockContent -> ConsHistory -> ConsHistory
insertCH (t,pos) bc ch = ch { chspace = M.adjust (M.adjust (first (const (Just bc))) pos) t (chspace ch)}

applyPWT :: Int -> ConsHistory -> PlayerWorldT -> MayFail ConsHistory
applyPWT _t _ch _pwt = success _ch -- TODO: implement applyPWT

{- MAIN FUNCTION: concreteHistory. should use runCondChecker -}
-- specializes all Unkowns to a unique history
-- or fails with contradictions.
-- this is called at the end of all inputs to check if the room is solved.
concreteHistory :: ConsHistory -> MayFail ConsHistory
concreteHistory _ = failing "TODO: implement concreteHistory"
-- for this function, inferencability from interactions blocks is nesessary (see Base.hs)
-- after everyhitn has been made concrete, another
-- run of checks will be done to assure that the inserted elements
-- didn't create new contradictions

foldlWithKeyM :: Monad m => (k -> a -> b -> m b) -> b -> M.Map k a -> m b
foldlWithKeyM f z = foldM (flip $ uncurry f) z . M.toAscList

-- TEST: consHistory (initGS pws) == initConsHistory _size pws

defaultConsHistory :: (Time,Pos) -> ConsHistory
defaultConsHistory (tmax,pmax) =
  CH {
     chspace = M.fromList $ (\t -> (t,) $ default2DMap pmax (Nothing,Nothing) ) <$> [0..tmax]
    ,chsize = (tmax,pmax)
  }

default2DMap :: Pos -> a -> Space a
default2DMap (sx,sy) e =
  M.fromList . concat $ map
                          (\y -> map
                                  (\x -> ((x,y),e))
                                  [0..sx])
                          ['A'..sy]
;

runTurn :: GameState -> S.Set (Specific PlayerTotal) -> MayFail GameState
runTurn = undefined
{-
the gamestate argument to runTurn is assumed to be self-consistent.

1. apply each players action
1.1. report if action is creates immediate contradiction
2. collect all observations
3. continue with next round
-}
