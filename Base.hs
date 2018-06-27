{-# LANGUAGE RankNTypes, ImpredicativeTypes, ExistentialQuantification, FlexibleContexts #-}

module Base
  where
  
-- for a general explanation:
-- see telegame workbook where the general approach
-- is explained in the pages before the last Startup Training Course notes

{-
Main todos:
. add anticipation
  . also needs current predictions for closed eyes anticipation
. implement game state tansition function

. rethink observations during transition (or put transition observations aside for first prototype?)
-}

import qualified Data.Set as S
import qualified Data.Map as M

{- coordinate system, x y -}
type DX = Int
type DY = Char 

type Pos = (DX,DY)
data Player = Player String Int Bool (S.Set PhyObj)
  deriving (Eq)
  {- name, age (steps since beginning), True means eyes are open, inventory -}

-- current observations of a specific player
data Specific a =
  Specific { -- the observations from the perspective of a specific player
     time :: Int
    ,currPlayerName :: String
    ,currPlayerTime :: Int
    ,observations :: a
  } -- using new identifier for the player: product of name and age of the player is uniquely determining
;

-- Specific OpenObs and Specific ClosedObs
-- as well as Specific ObenObs (for open eyes screen view)
-- and Specific RoomView (for closed eyes)
data OpenObs = OpenObs Map (S.Set (Pos,PhyObj)) (S.Set (Pos,Player))
data ClosedObs =
  ClosedObs
    Pos {- current position in map -}
    EnvObj {- env at current block -}
    (S.Set PhyObj)
    (S.Set Player)

{-
{- observations for transition phases -}
-- Specific OpenObsT and Specific ClosedObsT
-- where ´time´ corresponds to beginning time of the transition
 -- view of the room during time-transition
data OpenObsT =
  OpenObsT Map
    (S.Set (PhyOOT,PhyObj))  -- motion-of-object, object
    (S.Set (PlayerActionPhy,Player)) -- motion/action of player, player
data ClosedObsT =
  ClosedObsT Pos EnvObj EnvObj {- old env, new env -}
    (S.Set (PhyCOT,PhyObj)) (S.Set (PlayerActionPhyT,Player))
;
-}

-- what the screen shall show for a specific player.
-- during closed eyes this corresponds to predictions.
-- during opened eyes this corresponds to the ordinary open observations.
-- ActualScreenView = Specfic RoomView OR Specific OpenObs
-- data RoomView = entire room view isomorphic to OpenObs which is simply interpreted
-- as the prediction for the entire room.
-- the interactions with the current block become the observations.
--newtype ScreenView = Specific OpenObs

data PhyOOT = NoMotion | HorzAndFall Int
{- # of blocks to move to right and then gravity. -}

data PhyCOT = NoMotionT | MotionT Dir Dir
-- motion from directon to direction. e.g. Motion R D means, that it came from right and fell down at our block
-- not all possible values are legitimate. e.g. Motiong L U and Motion L L are invalid.
data Dir = L | U | R |D
  deriving (Show,Read)

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
-- instances in View.hs


data Map = Map {
   size :: Pos
  ,env :: M.Map Pos EnvObj
};

data PlayerActionTotal =
  PAT { eyesClosedBeg :: Bool -- True, if the eyes are closed in the beginning
        ,anticipationBeg :: ModifMap -- player can anticipate anything. though only their observations count
        ,phyAction :: PlayerActionPhy
        ,anticipationEnd :: ModifMap -- anticipation also needs to work for closed eyes roomview
        ,eyesClosedEnd :: Bool
        -- there are two anticipation points.
        -- both corresponding to the ancitipated change of something before or after the turn and movement.
        -- during closed eyes, the non-interfering prediction is shown as base for anticipation
  }
;

data Modif = M1 PhyObj | M2 Player

type ModifMap = M.Map Pos ([Modif],[Modif])
-- per Position: removals and additions

data PlayerActionPhy =
  MoveL | MoveR
  | JumpU | JumpUL | JumpUR
  | NoAction
  | Pick PhyObj
  | Put PhyObj
  | ThrowL { dist::Int, obj::PhyObj }
  | ThrowR { dist::Int, obj::PhyObj }
  | NewTOs Char
  | Teleport {
     channel :: Char
    ,sentObjects :: (S.Set Player, S.Set PhyObj)
    ,destTime :: Int
  }
;

data PlayerActionPhyT =
  MoveLT | MoveRT
  | JumpUT | FallT | NoActionT
  | PickT PhyObj
  | PutT PhyObj
  | ThrowLT PhyObj
  | ThrowRT PhyObj
  | NewTOsT Char
  | TeleportT {
      channelT :: Char,
      sentObjectsT :: (S.Set Player, S.Set PhyObj),
      isIncoming :: Bool -- True: incoming teleport, False: outgoing teleport
  }


type OObs = Specific OpenObs
type CObs = Specific ClosedObs
data PlayerState = PSO OObs OObs | PSC CObs OObs

type GameState = [PlayerState]
runTurn :: GameState -> [PlayerActionTotal] -> GameState
runTurn = undefined
