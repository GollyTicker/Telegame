{-# LANGUAGE TypeFamilies, DeriveFunctor,DeriveDataTypeable,FlexibleContexts #-}
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
. use a good haskell IDE to make types support writing even more?
  https://github.com/haskell/haskell-ide-engine
. use refinement types with liquid haskell to make better compile time assurance
  . or maybe simply stick to quickcheck? liquid haskell needs more time to learn though...
    especially, I want to "prove" properties between functions. and this is
    not immediately possible without existentials
  . add more properties and tests for quickcheck
. look at contradiction messages for the example and improve them.
  . make hunit tests? that would be nice
. add information, on whether a TimePos points to a St or Tr. use this while displaying contradiction
. use comonads abstraction or parsing for the condition checks?
. use blaze to generate the html? https://jaspervdj.be/blaze/tutorial.html
. 

count code size: cloc --exclude-ext=html,css src/

versions and packages:
. Haskell Platform Core. 8.4.3
  . using Z3 4.7.1 https://github.com/Z3Prover/z3/tree/z3-4.7.1
. stack 1.7.1: from pre-built binary https://github.com/commercialhaskell/stack/releases
. Haste (linux pre-built): pre-built package: https://haste-lang.org/downloads/
. quickcheck: http://www.cse.chalmers.se/~rjmh/QuickCheck/manual_body.html#16
. install new packages using haste-cabal instead of
normal cabal due to haste's cross compilation.
. it also uses a different ghc than the sys-wide installation.
. the haste-installation is found in /usr/local/lib/haste-compiler
. package multiset (version 0.3.4)
. NOT-USING-CURRENTLY: liquid haskell for backend verificaiton:https://github.com/ucsd-progsys/liquidhaskell/blob/develop/INSTALL.md

Stack:
. using two builds. one for running locally on console (main in Maps.hs),
and the other one running as html-webpage by haste.
  stack ghci, stack setup, stack build, stack exec ....

Liquid Haskell:
  . mini intro: https://ucsd-progsys.github.io/liquidhaskell-blog/
  . long book: http://ucsd-progsys.github.io/liquidhaskell-tutorial/01-intro.html#/sample-code
  . stack exec liquid src/Base.hs src/Interference.hs ...
-}

import Data.Proxy
import Data.Data
import qualified Data.Set as S
import Data.MultiSet (MultiSet)
import qualified Data.Map as M

{- coordinate system, x y -}
type Time = Int
type Pos = (Int,Char)
data Player = Player { pname :: String {- must be non-empty -}, peyes :: Bool, pinventory :: MultiSet PhyObj}
  deriving (Eq,Ord)
  {- name,(removed age for loops) , True means eyes are open, inventory -}

-- identificaiton based equality for players.
-- a player is identical to another player,
-- if they have the same name string and have the same age
eqById :: Player -> Player -> Bool
eqById (Player s {-_t-} _ _) (Player s' {-_t'-} _ _) = s == s' -- && _t == _t'

type PWorld a = OpenObs a
class (Show (This a),Typeable a) => Block a where
  type OpenObs a
  type ClosedObs a
  type Cons a
  type Antcpt a
  type Other a
  data This a
  this :: This a
  getter :: This a -> Field -> Maybe a
  on_standable :: a -> Bool
  in_standable :: a -> Bool
  permeable :: Show b => TimePos -> b -> a -> CondRes
  selfconsistent :: a -> CondRes
  interferesWithBlock :: TimePos -> a -> ConditionsChecker
  {-@ reduceToClosed  :: Proxy a -> OpenObs a -> ClosedObs a @-}
  reduceToClosed :: Proxy a -> OpenObs a -> ClosedObs a
  applypwObs :: Proxy a -> PWorld a -> (OpenObs a -> b) -> (ClosedObs a -> b) -> b
;

-- what the contents of a block can be.
-- during state and transition.
-- using multisets for objects, as they can occur multiple times
-- due to time-travel
data BlockSt = BC { bcps :: MultiSet Player, bcos :: MultiSet PhyObj, bcenv :: Env }
  deriving (Eq,Ord)
;

newtype BC_Cons = BCC BlockSt
  deriving (Ord,Eq)
-- OneP Player | OneO PhyObj | Bgrd Env
-- => e.g. OneP p1 & OneP p1 & OneP p2 & Bgrd (Door 0 0) <=>
-- BSCons MultiSet(p1,p1,p2) + Door 0 0 ==> ".P1. .P1. .P2. D00"
;

data BCT_Cons =
  BCTC {
    bctcInit :: Maybe Env
   ,bctcEnd  :: Maybe Env
   ,bctcToFutrP :: MultiSet Player
   ,bctcToFutrO :: MultiSet PhyObj
   ,bctcFromPastP :: MultiSet Player
   ,bctcFromPastO :: MultiSet PhyObj
} deriving (Eq,Ord)

data BlockTr = BCT {
    bctenv  :: (Env,EnvT,Env) -- old env, environment change, new env (possibly same)
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

-- motion from directon to direction. e.g. Motion R D means, that it came from right and fell down at our block
-- not all possible values are legitimate. e.g. Motiong L U and Motion L L are invalid.
data Dir = L | U | R |D
  deriving (Eq,Ord,Data,Typeable)
  
data PhyObjT = NoMotionT | MotionT Dir Dir
  | LandFrom Dir -- land from throws. Dir is L, R or U
  | IntoInventory {- also counting Door or switch -}
  | OntoGround {- also from switch or door -}
  | TParrive Char | TPexit Char -- when objects lying on ground are sent though tp.
  | TPsend | TPget -- teleorbs, when they are on ground, have TPsend for the
  -- source and TPget for the destination orb. both get destroyed then.
  deriving (Eq,Ord,Data,Typeable)

rundir :: a -> a -> a -> a -> Dir -> a
rundir l u r d dir = case dir of L -> l; U -> u; R -> r; D -> d

invDir :: Dir -> Dir
invDir = rundir R D L U

applyDir :: Dir -> TimePos -> TimePos
applyDir d (t,(x,y)) = case rundir (pred,id) (id,pred) (succ,id) (id,succ) d
  of (f,g) -> (,) t (f x,g y)

data PhyObj = TOrb Char Int {- identifier, int is 0 or 1 -}
            | Key
  deriving (Ord,Eq,Data,Typeable)

data Env = Door { dneeds :: Int, dhas :: Int } {- # keys needed, # keys inside. both have to be <=9 -}
  | Solid
  | Platform -- platform below current block
  | Blank
  | Switch { active :: Bool }  {- isActive -}
 {- | MovingBlock {
      mbDir :: Dir,
      mbMaxT :: Int,
      mbCurrT :: Int, -- both timers have to be <=99
      mcBehind :: Env
    } -}
  deriving (Eq, Ord,Data,Typeable)
;


data EAOnce = PressAndHold -- | more options later...
  deriving (Eq,Ord,Data,Typeable)
data EARep = TraverseDoor
  | InsertKey
  | TakeKey
  | ToogleSwitch
  deriving (Eq,Ord,Data,Typeable)
;
data EnvT = EnvStays | EnvUsedOnce EAOnce | EnvUsedMult [EARep]
  deriving (Eq,Ord,Data,Typeable)

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
  } -- deriving (Data,Typeable)
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
    ,tpsource :: TimePos -- tpos before start of teleportation.
    ,tpdest   :: TimePos -- tpos at finish of arrival. in case of normal space-tp: currTime+1
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
         (Char -> (MultiSet Player,MultiSet PhyObj) -> TimePos -> TimePos -> a) ->
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
             Teleport ch os ts td -> tele ch os ts td
;
toDirpa :: PlayerAction -> Maybe Dir
toDirpa = runpa (j L) (j R) (j U) (j U) (j U)
          n (\_->n) (\_->n) (\_ _->n) (\_ _->n) (\_->n) (\_->n) (\_->n) (\_ _ _ _->n)
  where
    n = Nothing; j = Just
;
-- called on a (Completed playerAction)
fromDirpa :: PlayerAction -> Maybe Dir
fromDirpa = runpa (j R) (j L) (j D) (j R) (j L)
          n (\_->n) (\_->n) (\_ _->n) (\_ _->n) (\_->n) (\_->n) (\_->n) (\_ _ _ _->n)
  where
    n = Nothing; j = Just
;
-- in addition to player actions,
-- during transition one can observe a few more things
-- in additions to the normals commands. they are complemented here
data PlayerT =
  Initiated PlayerAction
  -- | Intermediate PlayerAction -- e.g. MoveR for finish moving to right (before falling down now). obsolete due to Motion
  | Motion Dir Dir -- motion: incoming from and outgoing to
  {- possible values. L D , D R, D D, U D and their left-right symmetrical variants -}
  | Completed PlayerAction -- e.g. MoveR for arriving at the right block (an no subsq. falling)
      -- landing from a jump without falling is also counted here.
      -- if a non-moving action is executed (e.g. toogle switch),then always Completed is used.
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
type TeleportInfo = PlayerAction {- only Teleport case allowed -}
{- tpos before starting tp, teleorb-pair identifier, transfered objects, tpos after arrival -}
type CH_Global = M.Map Char TeleportInfo -- no in map is eqivalient to unknown.
data ConsHistory =
  CH {
    chspace  :: SpaceTime Field
   ,chsize   :: TimePos
   ,chglobal :: CH_Global 
  }
;
-- maxTime: maximum time for which the history is considered. on time progression
-- or future-time teleportation, this will be extended and filled with defaults.
-- The contraint matrix is indexed by
--   [t= 0..maxTime] X {State, Transition} X [p = (0,0)..chsize]
-- with the domain
--   (t,State,pos)       :: Maybe BlockSt  = Maybe (Set Player x Set PhyObj x Env)
--   (t,Transition,pos)  :: Maybe BlockTr for the transition starting at t
-- If the Maybe is Just, then the contents are uniquely determined.
-- If the Maybe is Nothing, then it is Unkown.
-- Inconsistent histories cannot be created in the first place.


type CondRes = String -- "" means satisfied. otherwise contradiction with description.
-- BIG NEXT TODO:
-- integrate with Cons BlockSt and Cons BlockTr.
-- make them so strong/expressive, that they can over all of what we need here.
-- perhaps, use both Cons to implement a new ConsHistory.
-- one which is made of minimal elements only. it only grows,
-- when condition-requirements from other places are checked.
-- new featureswill only be added, once they are needed.s
-- Choice (e.g. on standable vs in standable) will try to first
-- make any1 of them concrete and see, whether one can stay on them.
-- otherwise, the choice is deferred at a worse case of exp. time.
-- practically speaking, ground-check cannot become the feared exp-blowup,
-- because even in closed eyes, the current block is visible
-- and therefore, the requirement is always given or deferred to the lower block.
data ConditionsChecker =
  CC {
    ccneeds :: S.Set TimePos
   ,ccrun :: M.Map TimePos Field -> CH_Global -> [CondRes]
                      -- PRECONDITION: ccrun is called on a map,
                      -- which is defined for all keys in ccneeds!
 -- perhaps add output method to generate contradiction results?
 -- returns a CondRes for every check.
  }
;

