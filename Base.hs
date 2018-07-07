{-# LANGUAGE RankNTypes, ImpredicativeTypes, ExistentialQuantification, FlexibleContexts #-}

module Base
  where
  
-- for a general explanation:
-- see telegame workbook where the general approach
-- is explained in the pages before the last Startup Training Course notes

{-
Main todo:
. anticipation during opened eyes means, specification
of arrival of new players. it is (strictly) not the same as closing eyes
and anticipating a new player and opening the eyes.
Anticipation occurs in two variants.
Anticipation (of a state) and anticipationT(of a transition).

. implement constraint solving. need to extend constriant-solving
for incrementally added constraints where no backtracking is done
. implement game state tansition function
-}

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (intercalate)
-- import qualified Constraints as C -- add "-iConstraints" to ghc/i args and copy Constraints folder into this directory

{- coordinate system, x y -}
type Time = Int
type Pos = (Int,Char)
data Player = Player String Int Bool (S.Set PhyObj)
  deriving (Eq,Ord)
  {- name, age (steps since beginning), True means eyes are open, inventory -}

-- identificaiton based equality for players
eqById :: Player -> Player -> Bool
eqById (Player s t _ _) (Player s' t' _ _) = s == s' && t == t'

-- what the contents of a block can be.
-- during state and transition.
data BlockContent = BC { bcps :: (S.Set Player), bcos :: (S.Set PhyObj), bcenv :: EnvObj }
  deriving (Eq,Ord)
;

data BlockContentT = BCT {
    bctenvs :: (EnvObj,EnvObj,Maybe Dir) -- old env, new env, transition direction of a moving block, if relevant
   ,bctos   :: (S.Set (PhyCOT,PhyObj)) -- object motion
   ,bctps   :: (S.Set (PlayerActionT,Player))  -- player actions+motion
  }
  deriving (Eq,Ord)
  
-- Specific OpenObs and Specific ClosedObs
-- as well as Specific ObenObs (for open eyes screen view)
-- and Specific ClosedObs (for closed eyes)
type OpenObs = Sized BlockContent
type ClosedObs =
  (Pos{- current pos in map -}, BlockContent)

-- given a player-specific open-view, it reduces it to
-- the observations, the player would have, if there eyes were closed.
-- should be called on a player, that exists in ObenObs and whose eyes are closed.
reduceToClosed :: Specific OpenObs -> Specific ClosedObs
reduceToClosed spo@(Specific _ _ _ (Sized pos mp)) = spo {observations = (pos,mp M.! pos)}

-- analogous to above, just during transition.
-- the player is identified and their movements are traced to give the closed eyes observations.
-- the player should have their eyes closed.
reduceToClosedT :: Specific OpenObsT -> Specific ClosedObsT
reduceToClosedT spo@(Specific _ p_s p_t (Sized _ mp)) =
  spo { observations = M.filter (any (eqById (Player p_s p_t True S.empty) . snd) . bctps) mp }
  
  -- get all block-observations, where player is identified
;



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

-- observation during opened eyes. the entire map
type OpenObsT = Sized BlockContentT

{- observation, if the eyes are closed during transition -}
-- for an explanation: see Telegame workbook
-- We thus only need to collect a list of observed blicks/fields.
type ClosedObsT = M.Map Pos BlockContentT

-- current observations of a specific player
data Specific a =
  Specific { -- the observations from the perspective of a specific player
     time :: Int
    ,currPlayerName :: String
    ,currPlayerTime :: Int
    ,observations :: a
  } -- using new identifier for the player: product of name and age of the player is uniquely determining
 deriving (Eq, Ord)
;

data Sized a = Sized {
   size :: Pos, mapping :: M.Map Pos a
} deriving (Eq, Ord)


data PhyCOT = NoMotionT | MotionT Dir Dir
  deriving (Eq,Ord)
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
;
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
  deriving (Eq,Ord)
;

data EAOnce = PressAndHold -- | more options later...
  deriving (Eq,Ord)
data EARep = TraverseDoor
  | InsertKey
  | TakeKey
  | ToogleSwitch
  deriving (Eq,Ord)
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
  deriving (Eq,Ord)
;

-- what the screen shall show for a specific player.
-- during closed eyes this corresponds to predictions.
-- during opened eyes this corresponds to the ordinary open observations.
-- ActualScreenView = Specfic RoomView OR Specific OpenObs
-- data RoomView = entire room view isomorphic to OpenObs which is simply interpreted
-- as the prediction for the entire room.
-- the interactions with the current block become the observations.
--newtype ScreenView = Specific OpenObs

-- Specific OpenObs   = observations of the whole room from a specific players view
-- Specific ClosedObs = observations of the current pos from a specific players view

-- Specific OpenObsT = observations of the whole room from a specific players view during transition
-- Specific ClosedObsT = observations of the blocks a specific player visits and from their view during transition

-- a PlayerWorld has the information of how a room
-- looks to a player. the bool is True for opened eyes.
-- then the first attr corresponds to the world-view.
-- if the eyes are closed, then the first attr corresponds
-- to the predictions of the player. the actual observations
-- are the subset that is obtained by only looking at the
-- players position
data PlayerWorld = PW (Specific OpenObs) Bool
  deriving (Eq,Ord)
;
applypwObs :: PlayerWorld -> (Specific OpenObs -> a) -> (Specific ClosedObs -> a) -> a
applypwObs (PW sobs b) fo fc = if b then fo sobs else fc (reduceToClosed sobs)

data PlayerWorldT = PWT (Specific OpenObsT) Bool
