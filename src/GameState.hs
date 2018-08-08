{-# LANGUAGE ScopedTypeVariables,TupleSections,NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall #-}

module GameState (
     mkGSfromObs
    ,initGS
    ,runTurn
    ,finalizeHistory
    
    {- debug -}
    ,contradictions
  )
  where

import Interference
import View() -- Show instances for error messages
import qualified Data.Set as S
import qualified Data.Map as M
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Data.Foldable
--import Data.Maybe (listToMaybe)
import Control.Monad (foldM)
import Control.Arrow (first,second,(***))

-- import Debug.Trace

mkGSfromObs :: TotalObservations -> MayContra GameState
mkGSfromObs obs = fmap (GS obs) $ computeCHfromObs obs
  
-- creates initial gamestate from an initial set of player worlds
initGS :: MultiSet (PWorld BlockSt) -> MayContra GameState
initGS pws = mkGSfromObs (M.singleton 0 (pws,MS.empty))

{-
Assumptions:
. there is at least one time-step with an observation
  where a non-empty mapsize is used
-}
computeCHfromObs :: TotalObservations -> MayContra ConsHistory
computeCHfromObs obs = 
  do  
    let initCH = defaultConsHistory (maxT,mapSize)
    applyAllObservations obs initCH
  where mapSize = maybe (0,'A') (getSize . fst) $ M.minView obs
        getSize (pws,_) = case MS.elems pws of 
                      [] -> (0,'A')
                      (pw:_) -> ssize pw
        
        -- the maximum time is either the time of the latest observation
        -- or the latest time referenced in a teleportation
        maxT = 1 + max (maybe (-1) (fst.fst) $ M.maxViewWithKey obs) maxTeletime
        maximum' :: Foldable t => t Int -> Int
        maximum' = foldr max (-1)
        maxTeletime :: Int
        maxTeletime = maximum' $ M.map maxPerTime obs
        maxPerTime (_,pwts) = maximum' $ MS.map maxPerSpace pwts
        maxPerSpace = maximum' . M.map blockContent . sobservations
        blockContent :: BlockTr -> Int
        blockContent = maximum' . MS.map (maxDestTimePT . snd3) . bctps
        maxDestTimePT pat =
          runpat maxDestTimePA (\_ _ -> -1) maxDestTimePA (-1) pat
        maxDestTimePA (Teleport _ _ ts td) = fst ts `max` fst td
        maxDestTimePA _ = -1
;

{-      
1. create ConsHistory of maximal size
    size is equal to all maps size. we assume,
    that all maps are of same size.
    time goes from t=0 to 1+(maximal time mentioned).
    This can be the maximal time observed -
    but it can also be a higher time referenced in a teleportation.
    the 1+ is needed for contradictions check to ensure they don't fall out-of-bounds
  
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
applyAllObservations :: TotalObservations -> ConsHistory -> MayContra ConsHistory
applyAllObservations mp ch0 = foldlWithKeyM f ch0 mp
  where
    f :: Time -> (MultiSet (PWorld BlockSt), MultiSet (PWorld BlockTr)) -> ConsHistory -> MayContra ConsHistory
    f t (pws,pwts) ch =
      do ch2 <- foldM (applyPWSt t) ch  (toList pws )
         foldM        (applyPWTr t) ch2 (toList pwts)
;

-- assumes, that ch is consistent already.
-- it is inconcsistent, if (t,pos) is out-of-bounds
-- it is inconcsistent, if there is already a differing Just value at (t,pos)
-- it is consistent, if there is a Nothing at (t,pos) and
-- the network stays consistent after adding it.
applyPWSt :: Int -> ConsHistory -> PWorld BlockSt -> MayContra ConsHistory
applyPWSt t ch0 pw = applypwObs (Proxy::Proxy BlockSt) pw addOpenObs addClosedObs
  where
    addOpenObs = foldlWithKeyM (\pos bc -> addWhenConsistent (t,pos) bc) ch0 . sobservations
    addClosedObs x = let (pos,bc) = sobservations x in addWhenConsistent (t,pos) bc ch0
    addWhenConsistent :: TimePos -> BlockSt -> ConsHistory -> MayContra ConsHistory
    addWhenConsistent tpos bc ch =
         (\x -> insertCHSt tpos x ch) <$>
           atCHSt tpos (failPlayerObsOutOfBounds tpos ch)
                (maybe (success bc) (\bc' -> if bc == bc' then success bc else failPlayObsContraHistory tpos bc bc'))
                ch
;         -- these two functions are difficult to refactor together...
applyPWTr :: Int -> ConsHistory -> PWorld BlockTr -> MayContra ConsHistory
applyPWTr t ch0 pw = applypwObs (Proxy::Proxy BlockTr) pw addOpenObs addClosedObs
  where
    addOpenObs = foldlWithKeyM (\pos bc -> addWhenConsistent (t,pos) bc) ch0 . sobservations
    addClosedObs = foldlWithKeyM (\pos bc -> addWhenConsistent (t,pos) bc) ch0 . sobservations
    addWhenConsistent :: TimePos -> BlockTr -> ConsHistory -> MayContra ConsHistory
    addWhenConsistent tpos bc ch =
      do mybc  <- atCHTr tpos (failPlayerObsOutOfBounds tpos ch)
                  (maybe (success bc) (\bc' -> if bc == bc' then success bc else failPlayObsContraHistory tpos bc bc'))
                  ch
         chNew <- adjustGlobalInfo mybc ch -- also add information learned from the player obs. e.g. that a tp happend.
         return (insertCHTr tpos mybc chNew)
;

failPlayObsContraHistory :: (Show a, Show b) => TimePos -> a -> b -> MayContra c
failPlayObsContraHistory tpos b b' = failing $ "applyPW: players observation contradicts with established history at "++show tpos ++". observed: "++show b ++ ", established: " ++ show b'

failPlayerObsOutOfBounds :: TimePos -> ConsHistory -> MayContra a
failPlayerObsOutOfBounds tpos ch = failing $ "applyPW[unusual]: players observation "++show tpos++" is out-of-bounds in history. history size = "++ show (chsize ch) ++ ", with ch:\n" ++ show ch


-- TODO: consistency check can be made faster by memoizing
-- the bidirectional-dependencies. similarities to arc-consistency algo. for Constraint Solving Networks


-- returns the set of contradiction descriptions currently in the ConsHistory
contradictions :: ConsHistory -> [ConsResB]
contradictions ch = {- we assume that out-of-bounds is a problem. -}
  do let (maxT,(maxX,maxY)) = chsize ch
     t <- [0..maxT]
     x <- [0..maxX]
     y <- ['A'..maxY]
     let curr = (t,(x,y))
         checkbc  = maybe []{- unknowns ok -} (\(bc::BlockSt)  -> runChecks curr bc ch)
         checkbct = maybe []{- unknowns ok -} (\(bct::BlockTr) -> runChecks curr bct ch)
     uncurry (++) $ todo --atCHboth curr ["block(T) "++show curr++" out-of-bounds"] checkbc checkbct ch
;

{- MAIN FUNCTION: runChecks (from Block class) -}
runChecks :: Block b => TimePos -> b -> ConsHistory -> [ConsRes b]
runChecks curr b ch =
  let missing = findMissingIndices (cneeds cc) ch
      cc = interferesWithBlock curr b
  in  filter isContradiction $ selfconsistent b : 
        if null missing
          then [runCons cc (flatten (chspace ch)) (chglobal ch)]
          else map (listFailReferenceOutOfBounds curr) . toList $ missing
;

flatten :: Timed (Space a) -> M.Map TimePos a
flatten mp = M.fromAscList (M.toAscList mp >>= \(t,mpi) -> M.toAscList mpi >>= \(pos,x) -> [((t,pos),x)])

findMissingIndices :: S.Set TimePos -> ConsHistory -> S.Set TimePos
findMissingIndices ks = (ks S.\\) . M.keysSet . flatten . chspace

listFailReferenceOutOfBounds :: TimePos -> TimePos -> ConsRes b
listFailReferenceOutOfBounds curr other = notok $ show curr ++ " references out-of-bounds "++show other

atCHSt :: TimePos -> a {- out of bounds -} -> (Maybe BlockSt -> a) -> ConsHistory -> a
atCHSt tpos z f = fst . atCHboth tpos z f (const (error "atCH[1]: this cannot happen"))

atCHTr :: TimePos -> a {- out of bounds -} -> (Maybe BlockTr -> a) -> ConsHistory -> a
atCHTr tpos z f = snd . atCHboth tpos z (const (error "atCH[2]: this cannot happen")) f

-- combine two accesses into one
atCHboth :: TimePos -> a {- out of bounds -} -> (Maybe BlockSt -> a) -> (Maybe BlockTr -> a) -> ConsHistory -> (a,a)
atCHboth (t,pos) z f g = maybe (z,z) (f *** g) . (>>= M.lookup pos) . M.lookup t . chspace

-- inserts the blockContent into the cons-history.
-- assumes, that time-pos is not out-of-bounds
insertCHSt :: TimePos -> BlockSt -> ConsHistory -> ConsHistory
insertCHSt (t,pos) b ch = ch { chspace = M.adjust (M.adjust (first  (const (Just b))) pos) t (chspace ch)}

insertCHTr :: TimePos -> BlockTr -> ConsHistory -> ConsHistory
insertCHTr (t,pos) b ch = ch { chspace = M.adjust (M.adjust (second (const (Just b))) pos) t (chspace ch)}


{- MAIN FUNCTION: finalizeHistory. should use runChecks -}
-- specializes all Unkowns to a unique history
-- or fails with contradictions.
-- this is called at the end of all inputs to check if the room is solved.
finalizeHistory :: ConsHistory -> MayContra ConsHistory
finalizeHistory _ = failing "TODO: implement finalizeHistory"
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
     chspace  = M.fromList $ (\t -> (t,) $ default2DMap pmax (Nothing,Nothing) ) <$> [0..tmax]
    ,chsize   = (tmax,pmax)
    ,chglobal = M.empty
  }

default2DMap :: Pos -> a -> Space a
default2DMap (sx,sy) e = M.fromList $ (\x y -> ((x,y),e)) <$> [0..sx] <*> ['A'..sy]
;

extendCHTo :: Time -> ConsHistory -> ConsHistory
extendCHTo tMax ch@CH{chsize,chspace} =
  let new = M.union chspace $ M.fromList
        $ (,default2DMap (snd chsize) (Nothing,Nothing))
          <$> [fst chsize..tMax]
  in  ch{chspace = new}

{-
=======
{- IMPORTANT TODO:
    I need a way to allow multiple players to move at once.
    If only once player moves, then inserting their observations
    will lead to contradictions lateron on the other player - even
    for valid inputs.
    What can come to rescue, is that Observations may only be added
    in a batch for each Time. Then once all players have their inputs
    fixed, there is no room left and one can add all of their
    collective observations together.
    -}
{- USING: computeCHfromObs,
          data-type GameState,
          findPWorldInGameState
          inputToObs,
          noActionSucc
  ASSUME: gamestate has valid consistency history pre-computed
          and where the gamestate istself is already consistent.
  FUNCTION:
    runs all the players actions on the current gamestate at time
    t to t+1. It checks, that the map of actions truely contains
    all actionable players - otherwise not all actions are defined and
    it errors with "not enough actions".
    It might succeed with a new successive gamestate with
    new observations and an updates consistency history OR
    it returns contradiction warnings.
-}
runTurn :: GameState -> Time -> M.Map Player (MultiSet PlayerInput) -> MayContra GameState
runTurn gs0 t mmspi = undefined {-do
  pw <- findPWorldInGameState gs0 spi
  let obs = inputToObs (fmap (sobservations pw,) spi)
  addToObservations obs gs0-}
;

addToObservations :: (Time,PWorld BlockTr,PWorld BlockSt) -> GameState -> MayContra GameState
addToObservations (t,pobsT,pobs) (GS obs ch0) = do
  let newObs = M.fromList [(t  ,(MS.empty         , MS.singleton pobsT)),
                           (t+1,(MS.singleton pobs, MS.empty          ))]
      merge (ms1,mst1) (ms2,mst2) = (MS.union ms1 ms2,MS.union mst1 mst2)
  newCH <- applyAllObservations newObs (extendCHTo (t+1) ch0)
  return $ GS (M.unionWith merge obs newObs) newCH
;

{- todo: also add blocktr information to enable collaborative turn-taking -}
--findPWorldInGameState :: GameState -> Specific a -> MayContra (PWorld BlockSt)
findPWorldInGameState GS{gsobs} sp@Specific{stime,splayer} = do
  let mpwmpwt = do (pws,pwts) <- M.lookup stime gsobs
                   pw <- listToMaybe . toList . MS.filter (sameFocus sp) $ pws
               {- find any observations for the coming turn if already some players have moved -}
                   return pw
               --return $ (pw,) $ listToMaybe . toList . MS.filter (sameFocus sp) $ pwts
  maybeToEither
    [concat ["[t = ",show stime,"]: Player ",show splayer," not found"]]
    mpwmpwt
;
-}

{- USING: computeCHfromObs OK,
          data-type GameState OK,
          plyrInputAsObs OK
  ASSUME: gamestate has valid consistency history pre-computed
          and where the gamestate istself is already consistent.
  FUNCTION:
    adds the players inputs into the gamestate.
    either returns with immediate inconsistency warnings
    or returns a new updated gamestate.
    errors also, if inputs for any player in that turn is missing.
-}
runTurn :: MultiSet (Specific PlayerInput) -> GameState -> MayContra GameState
runTurn mspi gs0 = undefined {- todo:implement.

  pw <- findPWorldInGameState gs0 spi
  let obs = inputToObs (fmap (sobservations pw,) spi)
  addToObservations obs gs0 -}
;

