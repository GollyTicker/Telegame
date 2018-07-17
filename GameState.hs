{-# LANGUAGE ScopedTypeVariables #-}

module GameState
  where

import Base
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Foldable
import Data.Maybe (maybeToList)
import Control.Monad (foldM)


-- a gamestate contains all the memories and acitons of all the players as well as the environmental changes
-- it starts with the intial state of the players and adds an evolution
-- of transitions and successive states of the world and the players.
type TotalObservations = Timed (S.Set PlayerWorld, S.Set PlayerWorldT)
data GameState = GS {
    playerObs :: TotalObservations
       -- an intial player state for each player AND
      -- the history of the observations. each element in the sequence contains the 
      -- current player states as well as the transition observations following that state.

    ,consHistory :: ConsHistory
    -- a represented set of histories which are consistent with the current observations
  }
;

type MayFail a = Either String a

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

applyObservationsAtTime :: Time -> (S.Set PlayerWorld, S.Set PlayerWorldT) ->
                           ConsHistory -> MayFail ConsHistory
applyObservationsAtTime = undefined

foldlWithKeyM :: Monad m => (k -> a -> b -> m b) -> b -> M.Map k a -> m b
foldlWithKeyM f z = foldM (flip $ uncurry f) z . M.toAscList

computeCHfromObs :: TotalObservations -> MayFail ConsHistory
computeCHfromObs obs =
  do  
    let initCH = defaultConsHistory (maxT,mapSize)
    foldlWithKeyM applyObservationsAtTime initCH obs
  where mapSize = maybe (0,'A') (getSize . fst) $ M.minView obs
        getSize (pws,_) = case S.elems pws of 
                      [] -> (0,'A')
                      (pw:_) -> size . observations . pwobs $ pw
        
        -- the maximum time is either the time of the latest observation
        -- or the latest time referenced in a teleportation
        maxT = max (maybe (-1) (fst.fst) $ M.maxViewWithKey obs) maxTeletime
        maximum' :: Foldable t => t Int -> Int
        maximum' = foldr max (-1)
        maxTeletime :: Int
        maxTeletime = maximum' $ M.map maxPerTime obs
        maxPerTime (_,pwts) = maximum' $ S.map maxPerX pwts
        maxPerX = maximum' . M.map blockContent . mapping . observations . pwtobs
        blockContent :: BlockContentT -> Int
        blockContent = maximum' . S.map (maxDestTimePAT . fst) . bctps
        maxDestTimePAT pat =
          runpat maxDestTimePA maxDestTimePA (\_ _ -> -1) maxDestTimePA (-1) pat
        maxDestTimePA (Teleport _ _ t) = t
        maxDestTimePA _ = -1
;
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right 

{-      
1. create ConsHistory of maximal size
    size is equal to all maps size. we assume,
    that all maps are of same size.
    time goes from t=0 to the maximal time mentioned.
    This can be the maximal time observed - but
    it can also be a higher time referenced in a teleportation.
  
2. start at beginning and apply player actions one at a time

3. halt at first error (or at all errors)
-}

-- TEST: consHistory (initGS pws) == initConsHistory _size pws

type Field = (S.Set BlockContent, S.Set BlockContentT)
type TimePos = (Time,Pos)
type SpaceTime a = Timed (Space a)
collectContradictions :: ConsHistory -> S.Set (Time,Pos,Field)
collectContradictions ch@(CH m size) =
  S.fromList $
    do (t,pos) <- enumPosTime size
       field <- maybeToList (at (t,pos) ch)
       if isContradiction field
       then return (t,pos,field)
       else []
;

enumPos :: Pos -> [Pos]
enumPos (x,y) = [(x',y') | x' <- [0..x], y' <- ['A'..y]]

enumPosTime :: TimePos -> [TimePos]
enumPosTime (t,pos) = [ (t',pos') | t' <- [0..t], pos' <- enumPos pos]

isContradiction :: Field -> Bool
isContradiction (bc,bcT) = max (S.size bc) (S.size bcT) > 1

at :: (Time,Pos) -> ConsHistory -> Maybe Field
at (t,pos) ch = (M.lookup t $ getMatrix ch) >>= M.lookup pos

data ConsHistory =
  CH {
    getMatrix  :: SpaceTime Field
   ,chSize :: TimePos
  }
;
-- maxTime: maximum time for which the history is considered. on time progression
-- or future-time teleportation, this will be extended and filled with defaults.
-- The contraint matrix is indexed by
--   [t= 0..maxTime] X {State, Transition} X [p = (0,0)..chsize]
-- with the domain
--   (t,State,pos)       :: Set BlockContent  = Set (Set Player x Set PhyObj x EnvObj)
--   (t,Transition,pos)  :: Set BlockContentT = Set (BlockCObsT) for the transition starting at t
-- If the Set contains a single element, then that is the unique story.
-- If the Set contains multiple elements, then there is a contradiction,
-- because the content is required to be all elements at the same time
-- If the Set is empty (or Nothing is in the map), then it is unspecified.

-- given an initial gamestate, this will compute an initial
-- constraint solving network for latter use.
initConsHistory :: (Time,Pos) -> S.Set PlayerWorld -> ConsHistory
initConsHistory (t,p) ps =
  let dch = defaultConsHistory (t,p)
      newMat = foldr' f (M.map fst $ getMatrix dch M.! 0) ps
      f :: PlayerWorld -> Space (S.Set BlockContent) -> Space (S.Set BlockContent)
      f pw mp =
        applypwObs pw
          (\(Specific 0 _ _ (Sized _ obs)) -> addObs obs mp)
          (\(Specific 0 _ _ blkObs) -> addObs (M.fromList [blkObs]) mp)
      addObs mp mps = M.unionWith S.union (M.map S.singleton mp) mps
  in  dch { getMatrix = M.singleton 0 (M.map (\x -> (x,S.empty)) newMat) }
;

defaultConsHistory :: (Time,Pos) -> ConsHistory
defaultConsHistory (t,p) =
  CH {
     getMatrix = if t >= 0 then M.singleton 0 (default2DMap p (S.empty,S.empty)) else M.empty
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
