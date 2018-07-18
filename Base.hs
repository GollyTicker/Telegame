{-# LANGUAGE FlexibleContexts, DeriveFunctor #-}

module Base
  where
  
-- for a general explanation:
-- see telegame workbook where the general approach
-- is explained in the pages before the last Startup Training Course notes

{-
Main todo:
. implement game state tansition function
. how tosolve player identification problem?
  simply using an age is problematic for players in loops,
  where the age is cyclic.
  However, we can extend age to (Int,Int) and Int.
  The former means, that the playeris of age fst of
  a total maximum age of snd (at which age jumps back to 0).
  The latter means a normal monotonic growing age.


. clean up and tidy and refactor
-}

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (intercalate)

{- coordinate system, x y -}
type Time = Int
type Pos = (Int,Char)
data Player = Player { name :: String, age :: Int, eyesOp :: Bool, inventory :: (S.Set PhyObj)}
  deriving (Eq,Ord)
  {- name, age (steps since beginning), True means eyes are open, inventory -}

-- identificaiton based equality for players.
-- a player is identical to another player,
-- if they have the same name string and have the same age
eqById :: Player -> Player -> Bool
eqById (Player s t _ _) (Player s' t' _ _) = s == s' && t == t'

-- what the contents of a block can be.
-- during state and transition.
data BlockContent = BC { bcps :: (S.Set Player), bcos :: (S.Set PhyObj), bcenv :: EnvObj }
  deriving (Eq,Ord)
;

data BlockContentT = BCT {
    bctenvs :: (EnvObj,EnvObj,Maybe Dir) -- old env, new env, transition direction of a moving block, if relevant. only a single moving block may be relevant per grid-block
   ,bctos   :: (S.Set (PhyCOT,PhyObj)) -- object motion
   ,bctps   :: (S.Set (PlayerActionT,Player))  -- player actions+motion
  }
  deriving (Eq,Ord)
  
-- Specific BlockContent and Specific ClosedObs
-- as well as Specific BlockContent (for open eyes view)
-- and Specific (Pos,BlockContent) (with a single map entry) (for closed eyes)
type OpenObs = Specific (Space BlockContent)
type ClosedObs = Specific (Pos,BlockContent)

-- given a player-specific open-view, it reduces it to
-- the observations, the player would have, if there eyes were closed.
-- should be called on a player, that exists in ObenObs and whose eyes are closed.
reduceToClosed :: OpenObs -> ClosedObs
reduceToClosed spo@(Specific _ player _ mp) = spo {observations = (pos, mp M.! pos)}
  where xs = M.filter (any (eqById player) . bcps) $ mp
        pos = case (M.toList xs) of ((p,_):_) -> p ; [] -> error "reduceToClosed: Player not found"

-- analogous to above, just during transition.
-- the player is identified and their movements are traced to give the closed eyes observations.
-- the player should have their eyes closed.
reduceToClosedT :: OpenObsT -> ClosedObsT
reduceToClosedT spo@(Specific _ player _ mp) =
  M.filter (any (eqById player . snd) . bctps) mp
  
  -- get all block-observations, where player is identified
;

-- if the eyes are opened during the transition, then
-- the transition between intial and final state of the room
-- is recorded as observation.
-- if the eyes are opened/closed during the beginning/end of transition
-- then they are observed accordingly.
-- Thus it's possible to open the eyes for an infinitesimal
-- span of time inbetween consecutive turns.
-- Which means, that the player observed the state of the
-- room, when all was invisible.

{- observations for transition phases -}
-- Specific OpenObsT and Specific ClosedObsT
-- where ´time´ corresponds to beginning time of the transition
-- view of the room during time-transition

-- observation during opened eyes. the entire map
type OpenObsT = Specific (Space BlockContentT)

{- observation, if the eyes are closed during transition -}
-- for an explanation: see Telegame workbook
-- We thus only need to collect a list of observed blicks/fields.
type ClosedObsT = Space BlockContentT

type Timed a = M.Map Time a
type Space a = M.Map Pos a
-- current observations of a specific player of a room-view
data Specific a =
  Specific { -- the observations from the perspective of a specific player
     time :: Int
    ,currPlayer :: Player
    ,size :: Pos
    ,observations :: a
  } -- using identifier for the player: product of name and age of the player is uniquely determining
 deriving (Eq, Ord, Functor)
;

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
        ,anticipationBeg :: Anticipation -- player can anticipate anything. though only their observations count
        ,phyAction :: PlayerAction
        ,anticipationT :: AnticipationT  -- anticipate transitions
        ,anticipationEnd :: Anticipation -- anticipation also needs to work for closed eyes roomview
        ,eyesClosedEnd :: Bool
        -- there are two anticipation points.
        -- both corresponding to the ancitipated change of something before or after the turn and movement.
        -- during closed eyes, the non-interfering prediction is shown as base for anticipation
        -- AnticipationT describes anticipation of transitions
  }
-- the order of 'execution' is the order of the records in the declaration

type Anticipation = Space (Maybe BlockContent)
-- an anticipation concerns only the positions with Just.
-- these positions have a new BlockContent specified as the desired state

type AnticipationT = Space (Maybe BlockContentT)

data PlayerAction =
  MoveL | MoveR
  | JumpU | JumpUL | JumpUR
  | NoAction
  | Pick PhyObj | Put PhyObj
  | ThrowL { dist::Int, obj::PhyObj } | ThrowR { dist::Int, obj::PhyObj }
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

runpa :: a -> a ->
         a -> a -> a ->
         a -> 
         (PhyObj -> a) -> (PhyObj -> a) ->
         (Int -> PhyObj -> a) -> (Int -> PhyObj -> a) ->
         (Char -> a) ->
         (EAOnce -> a) ->
         ([EARep] -> a) ->
         (Char -> (S.Set Player,S.Set PhyObj) -> Int -> a) ->
         PlayerAction -> a
runpa ml mr ju jul jur na pk pt tl tr newto ueo uem tele pa =
  case pa of MoveL -> ml
             MoveR -> mr
             JumpU -> ju
             JumpUL -> jul
             JumpUR -> jur
             NoAction -> na
             Pick o -> pk o
             Put o -> pt o
             ThrowL d o -> tl d o
             ThrowR d o -> tr d o
             NewTOs c -> newto c
             UseEnvOnce eao -> ueo eao
             UneEnvMult eam -> uem eam
             Teleport ch os t -> tele ch os t
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

runpat :: (PlayerAction -> a) -> 
          (PlayerAction -> a) ->
          (Dir -> Dir ->   a) ->
          (PlayerAction -> a) ->
          a ->
          PlayerActionT ->
          a
runpat init inter intermot compl complf pat = 
  case pat of
    Initiated x -> init x
    Intermediate x -> inter x
    IntermediateMotion x y -> intermot x y
    Completed x -> compl x
    CompletedFalling -> complf
;
          

-- what the screen shall show for a specific player.
-- during closed eyes this corresponds to predictions.
-- during opened eyes this corresponds to the ordinary open observations.
-- ActualScreenView = Specfic RoomView OR Specific OpenObs
-- data RoomView = entire room view isomorphic to OpenObs which is simply interpreted
-- as the prediction for the entire room.
-- the interactions with the current block become the observations.
--newtype ScreenView = Specific OpenObs

-- OpenObs   = observations of the whole room from a specific players view
-- ClosedObs = observations of the current pos from a specific players view

-- OpenObsT = observations of the whole room from a specific players view during transition
-- ClosedObsT = observations of the blocks a specific player visits and from their view during transition

-- a PlayerWorld has the information of how a room looks to a player.
type PlayerWorld = OpenObs
type PlayerWorldT = OpenObsT

-- PlayerWorld  ~= Time x PlayerID x Space BlockContent
-- PlayerWorldT ~= Time x PlayerID x Space BlockContentT

applypwObs :: PlayerWorld -> (OpenObs -> a) -> (ClosedObs -> a) -> a
applypwObs sobs fo fc = if eyesOp (currPlayer sobs) then fo sobs else fc (reduceToClosed sobs)

