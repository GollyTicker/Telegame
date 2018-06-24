
module Base
  where
  
-- for a general explanation:
-- see telegame workbook where the general approach
-- is explained in the pages before the last Startup Training Course notes

{-
Main todos:
- add anticipation
-}

import qualified Data.Set as S
import qualified Data.Map as M

{- coordinate system, x y -}
type DX = Int
type DY = Char 

type Pos = (DX,DY)
data Player = Player String Int (S.Set PhyObj)
  {- name, age (steps since beginning), inventory -}

-- a view of the room as viewed from a concrete player
data RoomView =
  RoomView {
     time :: Int
    ,mapRV :: Map
    ,objects :: S.Set (Pos,PhyObj)
    ,players :: S.Set (Pos,Player)
    ,currPlayer :: (String,Int)
  } -- using new identifier for the player: product of name and age of the player is uniquely determining
;

-- a roomview per player during time-transition phase
data RoomViewT {-transition-} =
  RoomViewT {
     timeBeg :: Int
    ,mapRVT :: Map
    ,objectsT :: S.Set (PhyRVT,PhyObj) -- motion-of-object, object
    ,playersT :: S.Set (PlayerActionPhy,Player) -- motion/action of player, player
    ,currPlayerT :: (String,Int)
  }
data PhyRVT = NoMotion | HorzAndFall Int
{- # of blocks to move to right and then gravity. -}

-- current observations of a specific player
data CurrObs =
  OpenView RoomView
  | ClosedView Pos EnvObj {- env at current block -}
              (S.Set PhyObj) -- observations of objects and players at the current block
              (S.Set Player)
;

{- observations for transition phases -}
data CurrObsT =
  OpenViewT RoomViewT
  | ClosedViewT
      Pos EnvObj {- old env -} EnvObj {- new env -}
      (S.Set (PhyObsT,PhyObj))
      (S.Set (PlayerActionPhyT,Player))
;
data PhyObsT = NoMotionT | MotionT Dir Dir
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
  | MovingBlock
    { dir :: Dir,
      timerMax :: Int,
      timerCurrent :: Int, -- both timers have to be <=99
      behind :: EnvObj}
-- instances in View.hs


data Map = Map {
   size :: Pos
  ,env :: M.Map Pos EnvObj
};

data PlayerActionTotal =
  PAT { eyesClosedBeg :: Bool
        ,anticipationBeg :: () {- todo -}
        ,phyAction :: PlayerActionPhy
        ,anticipationEnd :: () {- todo -}
        ,eyesClosedEnd :: Bool
  }
;

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

-- this is also used as description of the action during transition phase

type GameState = [RoomView]

runTurn :: GameState -> [PlayerActionTotal] -> GameState
runTurn = undefined
