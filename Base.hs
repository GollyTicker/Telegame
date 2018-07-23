{-# LANGUAGE TypeFamilies, DeriveFunctor #-}
{-# OPTIONS_GHC -Wall #-}

module Base
  where


-- for a general explanation:
-- see telegame workbook where the general approach
-- is explained in the pages before the last Startup Training Course notes

{-
Main todo:
. implement game state tansition function
. clean up and tidy and refactor
. make web.interface. What to use?
  -> web-interface with Elm and interoperate with Haskell
    on server-side?
    -> using example https://github.com/haskell-servant/example-servant-elm
      -> FAIL cannot run on windows
  -> using haste.
    install https://github.com/valderman/haste-compiler/blob/master/doc/building.md
    -> great examples:
       1. http://ifeanyi.co/posts/client-side-haskell/
       2. 

versions and packages:
Haskell Platform Core. 8.4.3
package multiset (version 0.3.4)
-}

import Data.Proxy
import qualified Data.Set as S
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Data.List (intercalate)
import qualified Data.Map as M

{- coordinate system, x y -}
type Time = Int
type Pos = (Int,Char)
data Player = Player { pname :: String{-, page :: Int-}, peyes :: Bool, pinventory :: MultiSet PhyObj}
  deriving (Eq,Ord)
  {- name,(removed age for loops) , True means eyes are open, inventory -}

-- identificaiton based equality for players.
-- a player is identical to another player,
-- if they have the same name string and have the same age
eqById :: Player -> Player -> Bool
eqById (Player s {-_t-} _ _) (Player s' {-_t'-} _ _) = s == s' -- && _t == _t'

type PWorld a = OpenObs a
class Block a where
  type OpenObs a
  type ClosedObs a
  type Cons a
  type Antcpt a
  on_standable :: a -> Bool
  in_standable :: a -> Bool
  permeable :: TimePos -> a -> CondRes
  selfconsistent :: a -> CondRes
  interferesWithBlock :: TimePos -> a -> ConditionsChecker
  reduceToClosed :: Proxy a -> OpenObs a -> ClosedObs a
  applypwObs :: Proxy a -> PWorld a -> (OpenObs a -> b) -> (ClosedObs a -> b) -> b
;

-- what the contents of a block can be.
-- during state and transition.
-- using multisets for objects, as they can occur multiple times
-- due to time-travel
data BlockSt = BC { bcps :: MultiSet Player, bcos :: MultiSet PhyObj, bcenv :: EnvObj }
  deriving (Eq,Ord)
;

newtype BC_Cons = BCC BlockSt
  deriving (Ord,Eq)
-- OneP Player | OneO PhyObj | Bgrd EnvObj
-- => e.g. OneP p1 & OneP p1 & OneP p2 & Bgrd (Door 0 0) <=>
-- BSCons MultiSet(p1,p1,p2) + Door 0 0 ==> ".P1. .P1. .P2. D00"
;

data BCT_Cons =
  BCTC {
    bctcInit :: Maybe EnvObj
   ,bctcEnd  :: Maybe EnvObj
   ,bctcToFutrP :: MultiSet Player
   ,bctcToFutrO :: MultiSet PhyObj
   ,bctcFromPastP :: MultiSet Player
   ,bctcFromPastO :: MultiSet PhyObj
} deriving (Eq,Ord)

data BlockTr = BCT {
    bctenv  :: (EnvObj,EnvT,EnvObj) -- old env, environment change, new env (possibly same)
   ,bctos   :: M.Map PhyObj (MultiSet PhyObjT) -- object motion. since objects can be multiple, each object is identified with a multiset of motions
   ,bctps   :: MultiSet (Player,PlayerT,Player) -- player before -> action -> player after
  }
  deriving (Eq,Ord)
;
fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a
snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b
thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c

{-
IMPORTANT CONDITIONS:
An unknown BlockSt has to be uniquely determinable from
fully known neighboring BlockContentTs and distant blocks it interferes with (e.g. jump or teleport).
same holds for BlockTr.
=> using explicit constraints. BC_Cons and BCT_Cons
-}

-- Specific BlockSt and Specific ClosedObs
-- as well as Specific BlockSt (for open eyes view)
-- and Specific (Pos,BlockSt) (with a single map entry) (for closed eyes)

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
-- type OpenObsT = Specific (Space BlockTr)

{- observation, if the eyes are closed during transition -}
-- for an explanation: see Telegame workbook
-- We thus only need to collect a list of observed blicks/fields.
-- type ClosedObsT = Specific (Space BlockTr) -- == OpenObsT

type Timed a = M.Map Time a
type Space a = M.Map Pos a
-- current observations of a specific player of a room-view
data Specific a =
  Specific { -- the observations from the perspective of a specific player
     stime :: Int
    ,splayer :: Player
    ,ssize :: Pos
    ,sobservations :: a
  } -- beware. a player is not uniquely identified. multiple indistinguishable players might have the same view. but that'^s okay, because their observations will be identical as well.
 deriving (Eq, Ord, Functor)
;

data PhyObjT = NoMotionT | MotionT Dir Dir
  | LandFrom Dir -- land from throws. Dir is L, R or U
  | IntoInventory {- also counting Door or switch -}
  | OntoGround {- also from switch or door -}
  | TParrive Char | TPexit Char
  | TPsend | TPget
  -- when objects lying on ground are sent though tp.
  -- teleorbs, when they are on ground, have TPsend for the
  -- source and TPget for the destination orb. both get destroyed then.
  deriving (Eq,Ord)
-- motion from directon to direction. e.g. Motion R D means, that it came from right and fell down at our block
-- not all possible values are legitimate. e.g. Motiong L U and Motion L L are invalid.
data Dir = L | U | R |D
  deriving (Eq,Ord)

rundir :: a -> a -> a -> a -> Dir -> a
rundir l u r d dir = case dir of L -> l; U -> u; R -> r; D -> d

invDir :: Dir -> Dir
invDir = rundir R D L U

data PhyObj = TOrb Char Int {- identifier, int is 0 or 1 -}
            | Key
  deriving (Ord,Eq)

data EnvObj = Door { dneeds :: Int, dhas :: Int } {- # keys needed, # keys inside. both have to be <=9 -}
  | Solid
  | Platform -- platform below current block
  | Blank
  | Switch { active :: Bool }  {- isActive -}
 {- | MovingBlock {
      mbDir :: Dir,
      mbMaxT :: Int,
      mbCurrT :: Int, -- both timers have to be <=99
      mcBehind :: EnvObj
    } -}
  deriving (Eq, Ord)
;

data EnvT = EnvStays | EnvUsedOnce EAOnce | EnvUsedMult [EARep]
  deriving (Eq,Ord)

-- TODO: integrate PlayerActionTotal...
data PlayerTotal =
  PAT { eyesClosedBeg :: Bool -- True, if the eyes are closed in the beginning
        ,anticipationBeg :: Antcpt BlockSt -- player can anticipate anything. though only their observations count
        ,phyAction :: PlayerAction
        ,anticipationT :: Antcpt BlockTr  -- anticipate transitions
        ,anticipationEnd :: Antcpt BlockSt -- anticipation also needs to work for closed eyes roomview
        ,eyesClosedEnd :: Bool
        -- there are two anticipation points.
        -- both corresponding to the ancitipated change of something before or after the turn and movement.
        -- during closed eyes, the non-interfering prediction is shown as base for anticipation
        -- AnticipationT describes anticipation of transitions
  }
-- the order of 'execution' is the order of the records in the declaration

-- type Anticipation = Space (Maybe BlockSt)
-- an anticipation concerns only the positions with Just.
-- these positions have a new BlockSt specified as the desired state

-- type AnticipationT = Space (Maybe BlockTr)

data PlayerAction =
  MoveL | MoveR
  | JumpU | JumpUL | JumpUR
  | NoAction
  | Pick PhyObj | Put PhyObj
  | ThrowL { tldist::Int, tlobj::PhyObj } | ThrowR { trdist::Int, trobj::PhyObj }
  | NewTOs Char
  -- environment actions
  | UseEnvOnce EAOnce
  | UseEnvMult [EARep]
  -- multiple env. actions can be done at the same time.
  -- e.g. insert key and enter the door
  | Teleport {
     tpch :: Char
    ,tpobjs :: (MultiSet Player, MultiSet PhyObj) -- teleorbs of channel at source/dest. are not sent
    ,tpdesttime :: Int -- at finish of arrival. in case of normal space-tp: currTime+1
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
         (Char -> (MultiSet Player,MultiSet PhyObj) -> Int -> a) ->
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
             UseEnvMult eam -> uem eam
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
data PlayerT =
  Initiated PlayerAction
  -- | Intermediate PlayerAction -- e.g. MoveR for finish moving to right (before falling down now). obsolete due to Motion
  | Motion Dir Dir -- incoming and outgoing motion. e.g. move-away or jump.
  | Completed PlayerAction -- e.g. MoveR for arriving at the right block. landing from a jump without falling is also counted here
      -- if a non-moving action is executed (e.g. toogle switch),then Completed is used.
  | CompletedFalling -- completed falling is used, if the player finished their turn
  deriving (Eq,Ord)  -- on a different block than what ne would expect from the player action
;

runpat :: (PlayerAction -> a) -> 
        --(PlayerAction -> a) ->
          (Dir -> Dir ->   a) ->
          (PlayerAction -> a) ->
          a ->
          PlayerT ->
          a
runpat _init mot compl complf pat = 
  case pat of
    Initiated x -> _init x
  --Intermediate x -> inter x
    Motion x y -> mot x y
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
-- type PlayerWorld = OpenObs
-- type PlayerWorldT = OpenObsT

-- PlayerWorld  ~= Time x PlayerID x Space BlockSt
-- PlayerWorldT ~= Time x PlayerID x Space BlockTr

type MayFail a = Either String a

runMayFail :: (String -> a) -> (b -> a) -> MayFail b -> a
runMayFail = either

failing :: String -> MayFail a
failing = Left

success :: a -> MayFail a
success = Right

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right 




-- a gamestate contains all the memories and acitons of all the players as well as the environmental changes
-- it starts with the intial state of the players and adds an evolution
-- of transitions and successive states of the world and the players.
type TotalObservations = Timed (S.Set (PWorld BlockSt), S.Set (PWorld BlockTr))
data GameState = GS {
     gsobs :: TotalObservations
       -- an intial player state for each player AND
      -- the history of the observations. each element in the sequence contains the 
      -- current player states as well as the transition observations following that state.

    ,gsch :: ConsHistory
    -- a represented set of histories which are consistent with the current observations
  }
;
type Field = (Maybe BlockSt, Maybe BlockTr)
type TimePos = (Time,Pos)
type SpaceTime a = Timed (Space a)

data ConsHistory =
  CH {
    chspace  :: SpaceTime Field
   ,chsize :: TimePos
  }
;
-- maxTime: maximum time for which the history is considered. on time progression
-- or future-time teleportation, this will be extended and filled with defaults.
-- The contraint matrix is indexed by
--   [t= 0..maxTime] X {State, Transition} X [p = (0,0)..chsize]
-- with the domain
--   (t,State,pos)       :: Maybe BlockSt  = Maybe (Set Player x Set PhyObj x EnvObj)
--   (t,Transition,pos)  :: Maybe BlockTr for the transition starting at t
-- If the Maybe is Just, then the contents are uniquely determined.
-- If the Maybe is Nothing, then it is Unkown.
-- Inconsistent histories cannot be created in the first place.


type CondRes = String -- "" means satisfied. otherwise contradiction with description.
data ConditionsChecker =
  CC {
    ccneeds :: S.Set TimePos
   ,ccrun :: M.Map TimePos Field -> [CondRes] -- PRECONDITION: ccrun is called on a map,
                                              -- which is defined for all keys in ccneeds!
 -- perhaps add output method to generate contradiction results?
 -- returns a CondRes for every check.
  }
;


instance Show Player where -- added . at beginning and end for easier parsing
  show (Player s {-age-} o inv) = "." ++ f (s {- ++ show age -}) ++ invStr ++ "."
    where invStr | MS.null inv = ""
                 | otherwise  = "("++(intercalate "+" . map show . MS.toAscList $ inv)++")"
          f x = if not o then "<"++x++">" else x

instance Show PhyObj where
  show Key = "k"
  show (TOrb c i) = 't':c:show i
;