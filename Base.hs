{-# LANGUAGE RankNTypes, ImpredicativeTypes, ExistentialQuantification, FlexibleContexts #-}

module Base
  where
  
-- for a general explanation:
-- see telegame workbook where the general approach
-- is explained in the pages before the last Startup Training Course notes

{-
Main todo:
. implement game state tansition function
-}

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Sequence as Seq

{- coordinate system, x y -}
type DX = Int
type DY = Char 

type Pos = (DX,DY)
data Player = Player String Int Bool (S.Set PhyObj)
  deriving (Eq,Ord)
  {- name, age (steps since beginning), True means eyes are open, inventory -}

-- current observations of a specific player
data Specific a =
  Specific { -- the observations from the perspective of a specific player
     time :: Int
    ,currPlayerName :: String
    ,currPlayerTime :: Int
    ,observations :: a
  } -- using new identifier for the player: product of name and age of the player is uniquely determining
 deriving (Eq, Ord)

-- Specific OpenObs and Specific ClosedObs
-- as well as Specific ObenObs (for open eyes screen view)
-- and Specific RoomView (for closed eyes)
data OpenObs = OpenObs Map (S.Set (Pos,PhyObj)) (S.Set (Pos,Player))
  deriving (Eq, Ord)
data ClosedObs =
  ClosedObs
    Pos {- current position in map -}
    EnvObj {- env at current block -}
    (S.Set PhyObj)
    (S.Set Player)
  deriving (Eq, Ord)


{- observations for transition phases -}
-- Specific OpenObsT and Specific ClosedObsT
-- where ´time´ corresponds to beginning time of the transition
 -- view of the room during time-transition



-- if the eyes are opened during the transition, then
-- the transition between intial and final state of the room
-- is recorded as observation.
-- if the eyes are opened/closed during the beginning/end of transition
-- then they are observed accordingly.
-- Thus it's possible to open the eyes for an infinitesimal
-- span of time inbetween consecutive turns.
-- Which means, that the player observed the state of the
-- room, when all was invisible.

{- observation during opened eyes. state before and after the transition. -}
data OpenObsT = OObsT OpenObs OpenObs

{- observation, if the eyes are closed during transition -}
-- for an explanation: see Telegame workbook
-- We thus only need to collect a list of observed blicks/fields.
type ClosedObsT = [BlockCObsT]
data BlockCObsT =
  Pos -- observed block position
  (EnvObj,EnvObj,Maybe Dir) -- old env, new env, transition direction of a moving block, if relevant
  (S.Set (PhyCOT,PhyObj)) -- object motion
  (S.Set (PlayerActionT,Player)) -- player actions+motion
;

data PhyCOT = NoMotionT | MotionT Dir Dir
-- motion from directon to direction. e.g. Motion R D means, that it came from right and fell down at our block
-- not all possible values are legitimate. e.g. Motiong L U and Motion L L are invalid.
data Dir = L | U | R |D
  deriving (Show,Read,Eq,Ord)

data PhyObj = TOrb TeleOrb | Key
  deriving (Ord,Eq)

type TeleOrb = (Char,Int) {- identifier, first or second -}

data EnvObj = Door { needs :: Int, has :: Int } {- # keys needed, # keys inside. both have to be <=9 -}
  | Solid
  | Platform -- platform below current block
  | Blank
  | Switch { active :: Bool }  {- isActive -}
  | MovingBlock {
      dir :: Dir,
      timerMax :: Int,
      timerCurrent :: Int, -- both timers have to be <=99
      behind :: EnvObj
    }
  deriving (Eq, Ord)


data Map = Map {
   size :: Pos
  ,env :: M.Map Pos EnvObj
} deriving (Eq, Ord)

data PlayerActionTotal =
  PAT { eyesClosedBeg :: Bool -- True, if the eyes are closed in the beginning
        ,anticipationBeg :: ModifMap -- player can anticipate anything. though only their observations count
        ,phyAction :: PlayerAction
        ,anticipationEnd :: ModifMap -- anticipation also needs to work for closed eyes roomview
        ,eyesClosedEnd :: Bool
        -- there are two anticipation points.
        -- both corresponding to the ancitipated change of something before or after the turn and movement.
        -- during closed eyes, the non-interfering prediction is shown as base for anticipation
  }
;

data Modif = M1 PhyObj | M2 Player

type ModifMap = M.Map Pos (S.Set Modif,S.Set Modif)
-- per Position: removals and additions

data PlayerAction =
  MoveL | MoveR
  | JumpU | JumpUL | JumpUR
  | NoAction
  | Pick PhyObj
  | Put PhyObj
  | ThrowL { dist::Int, obj::PhyObj }
  | ThrowR { dist::Int, obj::PhyObj }
  | NewTOs Char
  -- environment actions
  | UseEnvOnce EAOnce
  | UneEnvMult [EARep]
  -- multiple env. actions can be done at the same time.
  -- e.g. insert key and enter the door
  | Teleport {
     channel :: Char
    ,sentObjects :: (S.Set Player, S.Set PhyObj)
    ,destTime :: Int
  }
;

data EAOnce = PressAndHold -- | more options later...
data EARep = TraverseDoor
  | InsertKey
  | TakeKey
  | ToogleSwitch
;


-- in addition to player actions,
-- during transition one can observe a few more things
-- in additions to the normals commands. they are complemented here
data PlayerActionT =
  Initiated PlayerAction
  | Intermediate PlayerAction -- e.g. MoveR for finish moving to right (before falling down now)
  | IntermediateMotion Dir Dir -- incoming and outgoing motion
  | Completed PlayerAction -- e.g. MoveR for arriving at the right block. landing from a jump without falling is also counted here
  | CompletedFalling
;

-- what the screen shall show for a specific player.
-- during closed eyes this corresponds to predictions.
-- during opened eyes this corresponds to the ordinary open observations.
-- ActualScreenView = Specfic RoomView OR Specific OpenObs
-- data RoomView = entire room view isomorphic to OpenObs which is simply interpreted
-- as the prediction for the entire room.
-- the interactions with the current block become the observations.
--newtype ScreenView = Specific OpenObs

type OObs = Specific OpenObs
type CObs = Specific ClosedObs
type OObsT = Specific OpenObsT
type CObsT = Specific ClosedObsT
data PlayerState = PSO OObs OObs | PSC CObs OObs
  deriving (Eq,Ord)

data PlayerStateT = PSOT OObsT | PSCT CObsT

-- a gamestate contains all the memories and acitons of all the players as well as the environmental changes
-- it starts with the intial state of the players and adds an evolution
-- of transitions and successive states of the world and the players.
-- each element in the sequence corresponds to an increase of time by 1.
-- e.g. initialGS corresponds to t=0.
-- Data.Sequence is used, because they are good in front+end access:
-- documentation: http://hackage.haskell.org/package/containers-0.5.6.2/docs/Data-Sequence.html
data GameState = GS {
     initialGS :: S.Set PlayerState -- an intial player state for each player
    ,history :: Seq.Seq (S.Set PlayerState, S.Set PlayerStateT)
      -- the history of the game. each element in the sequence contains the 
      -- current player states as well as thetransition observations leading to that state.
      -- the sequence is sorted from most-recent to oldest
  }
;
runTurn :: GameState -> S.Set (Specific PlayerActionTotal) -> GameState
runTurn = undefined
