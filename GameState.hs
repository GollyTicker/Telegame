{-# LANGUAGE ScopedTypeVariables #-}

module GameState
  where

import Base
import View -- for error messages
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Foldable
import Data.Maybe (maybeToList)
import Control.Monad (foldM)
import Control.Arrow (first) -- apply function on fst-element in tuple

mkGSfromObs :: TotalObservations -> MayFail GameState
mkGSfromObs obs = fmap (GS obs) $ computeCHfromObs obs
  
-- creates initial gamestate from an initial set of player worlds
initGS :: S.Set PlayerWorld -> MayFail GameState
initGS pws = mkGSfromObs (M.singleton 0 (pws,S.empty))

-- TODO: define this function
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
                      (pw:_) -> size pw
        
        -- the maximum time is either the time of the latest observation
        -- or the latest time referenced in a teleportation
        maxT = max (maybe (-1) (fst.fst) $ M.maxViewWithKey obs) maxTeletime
        maximum' :: Foldable t => t Int -> Int
        maximum' = foldr max (-1)
        maxTeletime :: Int
        maxTeletime = maximum' $ M.map maxPerTime obs
        maxPerTime (_,pwts) = maximum' $ S.map maxPerSpace pwts
        maxPerSpace = maximum' . M.map blockContent . observations
        blockContent :: BlockContentT -> Int
        blockContent = maximum' . S.map (maxDestTimePAT . fst) . bctps
        maxDestTimePAT pat =
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
applyAllObservations mp ch = foldlWithKeyM f ch mp
  where
    f :: Time -> (S.Set PlayerWorld, S.Set PlayerWorldT) -> ConsHistory -> MayFail ConsHistory
    f t (pws,pwts) ch = do ch2 <- foldM (applyPW  t) ch  (S.toList pws )
                           foldM (applyPWT t) ch2 (S.toList pwts)
;

-- TODO: continue
applyPW :: Int -> ConsHistory -> PlayerWorld -> MayFail ConsHistory
applyPW t ch pw = applypwObs pw addOpenObs undefined
  where
    addOpenObs = foldlWithKeyM (\pos bc -> addWhenConsistent (t,pos) bc) ch . observations
    addWhenConsistent :: TimePos -> BlockContent -> ConsHistory -> MayFail ConsHistory
    addWhenConsistent tpos bc ch =
      do finalBC <- atCH tpos (failPlayerObsOutOfBounds tpos ch)
              (maybe (success bc) (\bc' -> if bc == bc' then success bc else failPlayObsContraHistory tpos bc bc'))
              ch
         success $ insertCH tpos bc ch
;

failPlayObsContraHistory :: TimePos -> BlockContent -> BlockContent -> MayFail a
failPlayObsContraHistory tpos bc bc' = failing $ "applyPW: players observation contradicts with established history at "++show tpos ++". observed: "++show bc ++ ", established: " ++ show bc'

failPlayerObsOutOfBounds :: TimePos -> ConsHistory -> MayFail a
failPlayerObsOutOfBounds tpos ch = failing $ "applyPW[unusual]: players observation "++show tpos++" is out-of-bounds in history. history size = "++ show (chSize ch)

-- wereConsistent t pos bc ch returns true, if the history
-- ch with (t,pos) assigned to bc were consistent.
-- assumes, that ch is consistent already.
-- it is inconcsistent, if (t,pos) is out-of-bounds
-- it is inconcsistent, if there is already a differing Just value at (t,pos)
-- it is consistent, if there is a Nothing at (t,pos) and
-- the network stays consistent after adding it.
wereConsistent :: Time -> Pos -> BlockContent -> ConsHistory -> Bool
wereConsistent = undefined

-- TODO: consistency check can be made faster by memoizing
-- the bidirectional-dependencies. similarities to arc-consistency algo. for Constraint Solving Networks

-- returns the set of contradiction descriptions currently in the ConsHistory
contradictions :: ConsHistory -> [(TimePos,String)]
contradictions ch = {- we assume that out-of-bounds is a problem -}
  do let (maxT,(maxX,maxY)) = chSize ch
     t <- [0..maxT]
     x <- [0..maxX]
     y <- ['A'..maxY]
     let curr = (t,(x,y))
         checkbc = maybe [] (\bc -> runCondChecker (interferesWith curr bc) ch)
     atCH curr [(curr,"out-of-bounds")] checkbc ch
;

-- a condition checker is a list of tuples - which is defined for each BlockContent, if it were at time-pos TimePos
-- each tuple stands for a condition check at the element (time,pos) in the history.
-- the two functions describe conditions which are to hold
-- for the state and transition for the time and pos.
-- if the String is empty, then the condition is true. Otherwise it describes a contradiction.
type ConditionsChecker = [(Time,Pos,(Maybe BlockContent -> String),(Maybe BlockContentT -> String))]
interferesWith :: TimePos -> BlockContent -> ConditionsChecker
interferesWith = undefined

runCondChecker :: ConditionsChecker -> ConsHistory -> [(TimePos,String)]
runCondChecker = undefined

atCH :: TimePos -> a {- out of bounds -} -> (Maybe BlockContent -> a) -> ConsHistory -> a
atCH = undefined

-- inserts the blockContent into the cons-history.
-- assumes, that time-pos is not out-of-bounds
insertCH :: TimePos -> BlockContent -> ConsHistory -> ConsHistory
insertCH (t,pos) bc ch = ch { getMatrix = M.adjust (M.adjust (first (const (Just bc))) pos) t (getMatrix ch)}

applyPWT :: Int -> ConsHistory -> PlayerWorldT -> MayFail ConsHistory
applyPWT t ch pwt = failing "applyPWT not implemented"

-- specializes all Unkowns to a unique history
-- or fails with contradictions
concreteHistory :: ConsHistory -> MayFail ConsHistory
concreteHistory = undefined

foldlWithKeyM :: Monad m => (k -> a -> b -> m b) -> b -> M.Map k a -> m b
foldlWithKeyM f z = foldM (flip $ uncurry f) z . M.toAscList

-- TEST: consHistory (initGS pws) == initConsHistory _size pws

defaultConsHistory :: (Time,Pos) -> ConsHistory
defaultConsHistory (t,p) =
  CH {
     getMatrix = if t >= 0 then M.singleton 0 (default2DMap p (Nothing,Nothing)) else M.empty
    ,chSize = (t,p)
  }

default2DMap :: Pos -> a -> Space a
default2DMap (sx,sy) e =
  M.fromList . concat $ map
                          (\y -> map
                                  (\x -> ((x,y),e))
                                  [0..sx])
                          ['A'..sy]
;

runTurn :: GameState -> S.Set (Specific PlayerActionTotal) -> MayFail GameState
runTurn = undefined
{-
the gamestate argument to runTurn is assumed to be self-consistent.

1. apply each players action
1.1. report if action is creates immediate contradiction
2. collect all observations
3. continue with next round
-}
