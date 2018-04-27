
module Base
  where

import qualified Data.Set as S
import qualified Data.Map as M

type DX = Int
type DY = Char

type Pos = (DX,DY)
type Player = (String, S.Set PhyObj)
  {- name/UID and inventory -}

data RoomView =
  RoomView {
     time :: Int
    ,map :: Map
    ,objects :: S.Set (Pos,PhyObj)
    ,players :: S.Set (Pos,Player)
    ,currPlayer :: String
  }
;

{- want-to: needs also a roomview for transition phase -}


data CurrentObservations =
  OpenView RoomView
  | ClosedView EnvObj {- env at current block -}
              (S.Set RawObservation)
;
{- want-to: needs also observations for transition phases -}


data RawObservation =
  ObsPhyObj Pos PhyObj
  | ObsPlayer Pos Player

data PhyObj = TOrb TeleOrb | Key

type TeleOrb = (Char,Int) {- identifier, which -}

data EnvObj = Door { needs :: Int, has :: Int } {- numbr of keys needed, number of keys inside -}
  | Solid
  | Blank
  | Switch { active :: Bool }  {- isActive -}
  | MovingBlock
    { goesUp :: Bool,
      goesDown :: Bool,
      goesRight :: Bool,
      goesLeft :: Bool,
      timerMax :: Int,
      timerCurrent :: Int,
      behind :: EnvObj}

data Map = Map {
   sizeX :: DX
  ,sizeY :: DY
  ,env :: M.Map Pos EnvObj
};

data PlayerActionMnt

data PlayerActionPhy =
  MoveL | MoveR
  | JumpT | JumpTL | JumpTR
  | Pass
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
  


