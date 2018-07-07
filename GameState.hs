
module GameState
  where

import Base
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import Data.Foldable

{-
import Control.Monad.ST
import Control.Monad
import Data.STRef
-- using ST for local-immutable operations.
-- the M.Map can be replaced by mutable arrays
-- lateron for faster performance
-}

-- a gamestate contains all the memories and acitons of all the players as well as the environmental changes
-- it starts with the intial state of the players and adds an evolution
-- of transitions and successive states of the world and the players.
-- each element in the sequence corresponds to an increase of time by 1.
-- e.g. initialGS corresponds to t=0.
-- Data.Sequence is used, because they are good in front+end access:
-- documentation: http://hackage.haskell.org/package/containers-0.5.6.2/docs/Data-Sequence.html
data GameState = GS {
     initialGS :: S.Set PlayerWorld -- an intial player state for each player
    ,obsHistory :: Seq.Seq (S.Set PlayerWorld, S.Set PlayerWorld)
      -- the history of the observations. each element in the sequence contains the 
      -- current player states as well as thetransition observations leading to that state.
      -- the sequence is sorted from most-recent to oldest
    
    -- ,consHistory :: C.Net
      -- a represented set of histories which are consistent with the current initialGS and obsHistory
  }
;

data ConsHistory =
  CH {
    getMatrix  :: M.Map Time (M.Map Pos (S.Set BlockContent))
   ,getMatrixT :: M.Map Time (M.Map Pos (S.Set BlockContentT))
   ,chsize :: Pos
   ,maxTime :: Int
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
      newMat = foldr' f (getMatrix dch M.! 0) ps
      f :: PlayerWorld -> (M.Map Pos (S.Set BlockContent)) -> (M.Map Pos (S.Set BlockContent))
      f pw mp =
        applypwObs pw
          (\(Specific 0 _ _ (Sized _ obs)) -> addObs obs mp)
          (\(Specific 0 _ _ blkObs) -> addObs (M.fromList [blkObs]) mp)
      addObs mp mps = M.unionWith S.union (M.map S.singleton mp) mps
  in  dch { getMatrix = M.singleton 0 newMat }
;

defaultConsHistory :: (Time,Pos) -> ConsHistory
defaultConsHistory (t,p) =
  CH {
     getMatrix = M.singleton 0 (default2DMap p S.empty)
    ,getMatrixT = M.singleton 0 (default2DMap p S.empty)
    ,chsize = p
    ,maxTime = t
  }

default2DMap :: Pos -> a -> M.Map Pos a
default2DMap (sx,sy) e =
  M.fromList . concat $ map
                          (\y -> map
                                  (\x -> ((x,y),e))
                                  [0..sx])
                          ['A'..sy]
;

-- textual descriptions of contradictions found in a gamestate.
inconsistencies :: GameState -> [String]
inconsistencies gs = undefined

-- selfconsistent gs = null inconsistencies

type Failable a = Either String a
runTurn :: GameState -> S.Set (Specific PlayerActionTotal) -> Failable GameState
runTurn = undefined
{-
the gamestate argument to runTurn is assumed to be self-consistent.

1. apply each players action
1.1. report if action is creates immediate contradiction
2. collect all observations
3. continue with next round
-}
