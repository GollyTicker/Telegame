
module GameState
  where

import Base
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Sequence as Seq

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
     initialGS :: S.Set PlayerState -- an intial player state for each player
    ,obsHistory :: Seq.Seq (S.Set PlayerState, S.Set PlayerStateT)
      -- the history of the observations. each element in the sequence contains the 
      -- current player states as well as thetransition observations leading to that state.
      -- the sequence is sorted from most-recent to oldest
    
    -- ,consHistory :: C.Net
      -- a represented set of histories which are consistent with the current initialGS and obsHistory
  }
;

type Failable a = Either String a

data BlockContent = BC (S.Set Player) (S.Set PhyObj) EnvObj
type BlockContentT = BlockCObsT

data ConsHistory =
  CH {
    getMatrix  :: M.Map Time (M.Map Pos BlockContent )
   ,getMatrixT :: M.Map Time (M.Map Pos BlockContentT)
   ,chsize :: Pos
   ,maxTime :: Int
  }
-- maximum time for which the history is considered. on time progression
-- or future-time teleportation, this will be extended and filled with defaults.
-- The contraint matrix is indexed by
--   [t= 0..maxTime] X {State, Transition} X [p = (0,0)..chsize]
-- with the domain
--   (t,State,pos)       :: Maybe BlockContent  = Maybe (Set Player x Set PhyObj x EnvObj)
--   (t,Transition,pos)  :: Maybe BlockContentT = Maybe (BlockCObsT)
-- where Maybe a = Just a | Nothing
-- Since we are using Data.Map, we can encode an unspecified
-- block by keeping the entry empty i.e. Nothing
-- 

-- given an initial gamestate, this will compute an initial
-- constraint solving network for latter use.
initConsHistory :: (Time,Pos) -> S.Set PlayerState -> ConsHistory
initConsHistory (t,p) ps =
  let observations
      mat = undefined
      matT = undefined
  in  CH {
         getMatrix = mat
        ,getMatrixT = matT
        ,chsize = p
        ,maxTime = t
      }
;
{-initConsHistory :: (Time,Pos) -> S.Set PlayerState -> ConsHistory
initConsHistory (t,p) ps = 
 do matR <- newSTRef M.empty
    matTR <- newSTRef M.empty
    
    forM_ ps (\s)
    
    mat <- readSTRef matR
    matT <- readSTRef matTR
    return $
      CH {
         getMatrix = mat
        ,getMatrixT = matT
        ,chsize = p
        ,maxTime = t
      }
;-}

emptyConsHistory :: Pos -> Time -> ConsHistory
emptyConsHistory p t =
  CH {
     getMatrix = M.empty
    ,getMatrixT = M.empty
    ,chsize = p
    ,maxTime = t
  }
;

-- textual descriptions of contradictions found in a gamestate.
inconsistencies :: GameState -> [String]
inconsistencies gs = undefined

-- selfconsistent gs = null inconsistencies

runTurn :: GameState -> S.Set (Specific PlayerActionTotal) -> Failable GameState
runTurn = undefined
{-
the gamestate argument to runTurn is assumed to be self-consistent.

1. apply each players action
1.1. report if action is creates immediate contradiction
2. collect all observations
3. continue with next round
-}
