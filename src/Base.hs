{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Base(
     module Data.Proxy
    ,todo
    ,Time,Pos,TimePos
    ,Dir(..)
    ,rundir
    ,invDir
    ,applyDir
    ,EAOnce(..)
    ,EARep(..)
    ,PhyObj(..)
    ,PhyObjT(..)
    ,afterPhyObjT
    ,Env(..)
    ,EnvT(..)
    ,Player(..)
    ,PlayerAction(..)
    ,Teleport(..)
    ,runpa
    ,toDirpa
    ,fromDirpa
    ,PlayerT(..)
    ,runpat
    ,n -- Nothing
    ,j -- Just
  )
  where

-- for a general explanation:
-- see telegame workbook where the general approach
-- is explained in the pages before the last Startup Training Course notes

{-
        ████████  █████  ███████ ██   ██ ███████
           ██    ██   ██ ██      ██  ██  ██
           ██    ███████ ███████ █████   ███████
           ██    ██   ██      ██ ██  ██       ██
           ██    ██   ██ ███████ ██   ██ ███████
-}

{-
Main todo:
. implement `runTurn` function
. make basic puzzle runnable from finish to end
  using hard-coded PlayerInputActions

. finish semantics of constraints in Interference.hs
. make code easier to understand.
. replace instances BlockSt/Tr by St/Tr. clearer view.
. clean up and tidy and refactor
. INTEGRATE stack into Haste. unified and reproduceable build
. use a good haskell IDE to make types support writing even more?
  https://github.com/haskell/haskell-ide-engine
. look at contradiction messages for the example and improve them.
. great idea for touch-input:
  . when mobile browser, make only static screens rather than
    scrollable screen. then allow for touch-gestures to more easily input
    actions. e.g. touch on a player to see info. when that player is
    selected, a swipe on the screen which goes ^ and > will be parsed as
    JumpUR. then it will be executed immediately, if all other infos are given.
    one could also use a dragable circle which makes it clearer, which action
    one is inputting.

count code size in /src : cloc --exclude-ext=html,js,sh,css .

versions and packages:
. Haskell Platform Core. 8.4.3
. stack 1.7.1: from pre-built binary https://github.com/commercialhaskell/stack/releases
. Haste: pre-built package: https://haste-lang.org/downloads/
. quickcheck: http://www.cse.chalmers.se/~rjmh/QuickCheck/manual_body.html#16
. install new packages using haste-cabal instead of
normal cabal due to haste's cross compilation.
. it also uses a different ghc than the sys-wide installation.
. the haste-installation is found in /usr/local/lib/haste-compiler
. package multiset (version 0.3.4)

Stack:
. using two builds. one for running locally on console (main in Maps.hs),
and the other one running as html-webpage by haste.
  stack ghci, stack setup, stack build, stack exec ....

Licensing:
  . Haste BSD-3-Clause http://hackage.haskell.org/package/haste-compiler-0.6.0.0/src/LICENSE
  . Stack BSD-3-Clause https://github.com/commercialhaskell/stack/blob/master/LICENSE
  . QuickCheck BSD-3-Clause
-}

import Data.Proxy
import Data.Data
import Data.MultiSet (MultiSet)

todo :: a
todo = undefined


{-
        ██████   █████  ███████ ███████     ████████ ██    ██ ██████  ███████ ███████
        ██   ██ ██   ██ ██      ██             ██     ██  ██  ██   ██ ██      ██
        ██████  ███████ ███████ █████          ██      ████   ██████  █████   ███████
        ██   ██ ██   ██      ██ ██             ██       ██    ██      ██           ██
        ██████  ██   ██ ███████ ███████        ██       ██    ██      ███████ ███████
-}

{- coordinate system, x y -}
type Time = Int
type Pos = (Int,Char)
type TimePos = (Time,Pos)

data Dir = L | U | R |D
  deriving (Eq,Ord,Data,Typeable)
;

data PhyObj = TOrb Char Int {- identifier, int is 0 or 1 -}
            | Key
    -- could model destroyed tele-orbs here. they would be useful lateron anyways.
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
  deriving (Eq,Ord,Data,Typeable)
;

data Player = Player { pname :: String {- must be non-empty -}, peyes :: Bool, pinventory :: MultiSet PhyObj}
  deriving (Eq,Ord)
  {- name,(removed age for loops) , True means eyes are open, inventory -}

data PhyObjT = NoMotionT | MotionT Dir Dir {- valids: LD,LR,UD,RD,RL-}
  | LandFrom Dir -- land from throws. Dir is L, R or U
  | IntoInventory {- also counting Door or switch. is this okay like this? without info on interacting object? -}
  | OntoGround {- also from switch or door -}
  | TParrive Teleport | TPexit Teleport -- when objects lying on ground are sent though tp.
  | TPsend | TPget -- teleorbs, when they are on ground, have TPsend for the
  -- source and TPget for the destination orb. both get destroyed then.
  deriving (Eq,Ord)

data EnvT = EnvStays | EnvUsedOnce EAOnce | EnvUsedMult [EARep]
  deriving (Eq,Ord,Data,Typeable)
;

data EAOnce = PressAndHold -- | more options later...
  deriving (Eq,Ord,Enum,Data,Typeable)
data EARep = TraverseDoor
  | InsertKey
  | TakeKey
  | ToogleSwitch
  deriving (Eq,Ord,Data,Typeable)
;

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
  -- player activates teleorb. may send themselves as well.
  | TP Bool Teleport {- tpinfo, bool: activator is sent -}
  -- player arrives or leaves via teleportation.
  | TParriveP Teleport | TPexitP Teleport
  deriving (Eq,Ord)
;

data Teleport = Teleport {
     tpch :: Char
    ,tpobjs :: (MultiSet Player, MultiSet PhyObj) -- teleorbs of channel at source/dest. are not sent.
      -- we use the state of the objects at the beginning of the teleport
      -- this only makes a difference for the tp-activating player who loses their teleorb on tp.
      -- currently, we only support activating teleorbs from ground. Hence inventory change doesn't matter.
    ,tpsource :: TimePos -- tpos before start of teleportation. tp in trans t.
    ,tpdest   :: TimePos -- tpos at finish of arrival. in case of normal space-tp: t=currTime+1. tp in trans t-1 == currTime.
  }
  deriving (Eq,Ord)

-- in addition to player actions,
-- during transition one can observe a few more things
-- in additions to the normals commands. they are complemented here
data PlayerT =
  Initiated PlayerAction
  | Motion Dir Dir -- motion: incoming from and outgoing to
  {- possible values. L D , D R, D D, U D and their left-right symmetrical variants -}
  | Completed PlayerAction -- e.g. MoveR for arriving at the right block (an no subsq. falling)
      -- landing from a jump without falling is also counted here.
      -- if a non-moving action is executed (e.g. toogle switch),then always Completed is used.
  | CompletedFalling -- completed falling is used, if the player finished their turn
  deriving (Eq,Ord)  -- on a different block than what ne would expect from the player action
;


{- util functions for base data-types -}

runpa :: a -> a ->
         a -> a -> a ->
         a ->
         (PhyObj -> a) -> (PhyObj -> a) ->
         (Int -> PhyObj -> a) -> (Int -> PhyObj -> a) ->
         (Char -> a) ->
         (EAOnce -> a) ->
         ([EARep] -> a) ->
         (Bool -> Teleport -> a) ->
         (Teleport -> a) ->
         (Teleport -> a) ->
         PlayerAction -> a
runpa ml mr ju jul jur na pk pt tl tr newto ueo uem tele tpa tpe pa =
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
             TP b tp -> tele b tp
             TParriveP tp -> tpa tp
             TPexitP tp -> tpe tp
;
toDirpa :: PlayerAction -> Maybe Dir
toDirpa = runpa (j L) (j R) (j U) (j U) (j U)
          n (\_->n) (\_->n) (\_ _->n) (\_ _->n) (\_->n) (\_->n) (\_->n) (\_ _->n) (\_ ->n) (\_ ->n)

;
-- called on a (Completed playerAction)
fromDirpa :: PlayerAction -> Maybe Dir
fromDirpa = runpa (j R) (j L) (j D) (j R) (j L)
          n (\_->n) (\_->n) (\_ _->n) (\_ _->n) (\_->n) (\_->n) (\_->n) (\_ _->n) (\_ ->n) (\_ ->n)
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

rundir :: a -> a -> a -> a -> Dir -> a
rundir l u r d dir = case dir of L -> l; U -> u; R -> r; D -> d

invDir :: Dir -> Dir
invDir = rundir R D L U

applyDir :: Dir -> TimePos -> TimePos
applyDir d (t,(x,y)) = case rundir (pred,id) (id,pred) (succ,id) (id,succ) d
  of (f,g) -> (,) t (f x,g y)

runPhyObjT :: a
  -> (Dir -> Dir -> a)
  -> (Dir -> a)
  -> a -> a
  -> (Teleport -> a) -> (Teleport -> a)
  -> a -> a
  -> PhyObjT -> a
runPhyObjT nomot mot lf inv grd tparr tpext tpsnd tpget o = case o of
  NoMotionT        -> nomot
  MotionT din dout -> mot din dout
  LandFrom di      -> lf di
  IntoInventory    -> inv
  OntoGround       -> grd
  TParrive c       -> tparr c
  TPexit   c       -> tpext c
  TPsend           -> tpsnd
  TPget            -> tpget
;

afterPhyObjT :: PhyObj -> PhyObjT -> Maybe PhyObj
afterPhyObjT o = runPhyObjT (j o) (\_ _->n) (\_->j o) n (j o) (\_->j o) (\_->n) n n

n :: Maybe a
n = Nothing

j :: a -> Maybe a
j = Just
