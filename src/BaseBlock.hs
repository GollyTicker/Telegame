{-# LANGUAGE TupleSections,ExistentialQuantification,NamedFieldPuns,FlexibleContexts,TypeFamilies,DeriveFunctor,FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module BaseBlock (
     module Base
    ,Block(..)
    ,BlockSt(..)
    ,BlockTr(..)
    ,fst3,snd3,thd3
    ,PWorld
    ,Timed
    ,Space
    ,Specific(..)
    ,sameFocus
    ,noAction
    ,MayContra
    ,runMayContra
    ,failing
    ,success
    ,maybeToEither
    ,TotalObservations
    ,GameState(..)
    ,Field
    ,TeleportInfo
    ,CH_Global
    ,CH_GlobalP
    ,unknownGlobalP
    ,ConsHistory(..)
    ,ConsDesc
    ,STCons
    ,ConsHistoryP(..)
    ,Expr,eLeaf,eAll,eAny
    ,alwaysOk,neverOk,orElse,also,implies,fromBool
    ,foldExpr
    ,ConsRes
    ,PlayerInput(..)
  ) where

import Base
import Data.Data
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import qualified Data.Map as M

{- general class to put together BlockTr and BlockSt. instantiations in Inference.hs -}
type PWorld a = OpenObs a
class (Show (This a),Typeable a) => Block a where
  type OpenObs a
  type ClosedObs a
  data Cons a
  {- explicit minimality constraint representation of BlockSt/BlockTr -}
  {-
  IMPORTANT CONDITIONS:
  An unknown BlockSt has to be uniquely determinable from
  fully known neighboring BlockContentTs and distant blocks it interferes with (e.g. jump or teleport).
  (unless contradiction of course).
  same holds for BlockTr.
  => using explicit constraints. BC_Cons and BCT_Cons
  -}
  type Antcpt a
  data This a
  this :: This a
  getter :: This a -> Field -> Maybe a
  setter :: This a -> Cons a -> (Cons BlockSt,Cons BlockTr)
  on_standable :: This a -> TimePos -> ConsDesc -> STCons
  in_standable :: This a -> TimePos -> ConsDesc -> STCons
  permeable :: This a -> TimePos -> ConsDesc -> STCons
  leastc :: Cons a
  blockConstraints :: TimePos -> a -> STCons
  reduceToClosed :: Proxy a -> OpenObs a -> ClosedObs a
  applypwObs :: Proxy a -> PWorld a -> (OpenObs a -> b) -> (ClosedObs a -> b) -> b
;

{- contents of a state block. multiset, because the same obj/plyr can occur identically many times -}
data BlockSt = BC {
    bcenv :: Env,
    bcos :: MultiSet PhyObj,
    bcps :: MultiSet Player
  }
  deriving (Eq,Ord)
;
data BlockTr = BCT {
    bctenv  :: (Env,EnvT,Env) -- old env, environment change, new env (possibly same)
   ,bctos   :: MultiSet (PhyObj,PhyObjT,PhyObj) -- object change. similar to player.
   ,bctps   :: MultiSet (Player,PlayerT,Player) -- player before -> action -> player after.
  } -- if the player/object is created/destroyed,
    -- then the first/last element in tuple is equal to the things initial/last state.
    -- semantically, that element represents what the thing was/became before/after creation/destruction.
  deriving (Eq,Ord)

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a
snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b
thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c

-- Specific BlockSt and Specific ClosedObs
-- as well as Specific BlockSt (for open eyes view)
-- and Specific (Pos,BlockSt) (with a single map entry) (for closed eyes)

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

{- current observations of a specific player of a (room-)view -}
data Specific a =
  Specific { -- the observations from the perspective of a specific player
     stime :: Time
    ,splayer :: Player
    ,ssize :: Pos
    ,sobservations :: a
  } -- beware. a player is not uniquely identified. multiple indistinguishable players might have the same view. but that's okay, because their observations will be identical as well.
 deriving (Eq, Ord, Functor)
;
sameFocus :: Specific a -> Specific b -> Bool
sameFocus sp1 sp2 =
  stime sp1 == stime sp2
  && splayer sp1 == splayer sp2
  && ssize sp1 == ssize sp2


{- MAY CONTRADICT: type for a possibly contradiction-raising vale -}
type MayContra a = Either [ConsDesc] a

runMayContra :: ([ConsDesc] -> a) -> (b -> a) -> MayContra b -> a
runMayContra = either

failing :: String -> MayContra a
failing = Left . (:[])

success :: a -> MayContra a
success = Right

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right



{-
         ██████   █████  ███    ███ ███████ ███████ ████████  █████  ████████ ███████
        ██       ██   ██ ████  ████ ██      ██         ██    ██   ██    ██    ██
        ██   ███ ███████ ██ ████ ██ █████   ███████    ██    ███████    ██    █████
        ██    ██ ██   ██ ██  ██  ██ ██           ██    ██    ██   ██    ██    ██
         ██████  ██   ██ ██      ██ ███████ ███████    ██    ██   ██    ██    ███████
-}
-- a gamestate contains all the views+thoughts and actions of each player at each time as well as the environmental changes
-- it starts with the intial state of the players and adds an evolution
-- of transitions and successive states of the world and the players.
type TotalObservations = Timed (MultiSet (PWorld BlockSt),MultiSet (PWorld BlockTr))
data GameState = GS {
     gsobs :: TotalObservations
       -- an intial player state for each player AND
      -- the history of the observations. each element in the sequence contains the
      -- current player states as well as the transition observations following that state.

    ,gsch :: ConsHistory
    -- a represented set of histories which are consistent with the current observations
  }
;


{- ============   CONSISTENCY HISTORY =============== -}
type Field = (Maybe BlockSt, Maybe BlockTr)
type TeleportInfo = PlayerAction {- only Teleport case allowed -}
{- tpos before starting tp, teleorb-pair identifier, transfered objects, tpos after arrival -}
type CH_Global = M.Map Char Teleport -- no in map is eqivalient to unknown.
unknownGlobalP :: CH_GlobalP
unknownGlobalP = M.empty
type CH_GlobalP = M.Map Char ({-Partial-} Teleport)
data ConsHistory =
  CH {
    chspace  :: Timed (Space Field)
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


{-
         ██████  ██████  ███    ██ ███████ ████████ ██████   █████  ██ ███    ██ ████████ ███████
        ██      ██    ██ ████   ██ ██         ██    ██   ██ ██   ██ ██ ████   ██    ██    ██
        ██      ██    ██ ██ ██  ██ ███████    ██    ██████  ███████ ██ ██ ██  ██    ██    ███████
        ██      ██    ██ ██  ██ ██      ██    ██    ██   ██ ██   ██ ██ ██  ██ ██    ██         ██
         ██████  ██████  ██   ████ ███████    ██    ██   ██ ██   ██ ██ ██   ████    ██    ███████
-}

{-
TODO: invsertigate, whether it makes sense to replace ConsHistory
  by ConsHistoryP. then we have a finer level of observations and uncertainity.
  this fits to the lower comment of making player-obervations also more
  fine-grained.

  => eventually implemented by reducing to minimal observable shards -
  which will become identical to atomic player/physicalObject states and transitions

Distinction:
1. PlayerActions. each player action, applied on a CondHistory
   can only spawn observations in the (St,Tr,St) surrounding
   that action.

2. PlayerObservations (like [? S]). they only
   limit what actually already happend. steps in immediate future
   are not observed yet. only current step (St or Tr) is observed.
   cannot create contradictions. just observations.
   contradicting observations (even in the same player) are only
   discovered later.
   IMP. REALIZATION:
   conditions on interacted with elements from current time are enforced.
   e.g. reduced from blurred prediction to actual observation.
   as mentioned above, if they are in a different field, then it's only a
   partial observation.
   TODO: perhaps change this? maybe let them only observe the things they
   actually interact with. thus, they are also closed to their current field.
   makes understanding this more intuitive.
   However, this may only enforce some level of 'careful walking' for
   interactions form future.
   The implementation will make use of Interference.hs to get the
   set of conditions, which need to be observed in the current time.

      Partial observations might be displayed as:
      ?,  P,?
      ?,? S,?
      denoting, that the player knows, that there is a S block below him
      but doesn't know it's other contents.

   Thus we need to change the semantics of playerObservations
   to encode full and partial observations. actually, partial
   observations suffice where a full observation is just a large set
   of partial observations + a flag to denote, that everything was observed.
   => Could Use Cons BlockSt/Tr and ConsHistoryP

3. for each object -> get countable and constructable constraints
   on self-block and other blocks and global information.
   these don't force an unknown to become known.

4. for each checkable (block + global info) collect all of the constraints
   from previous step and make ConsHistory a bit more concrete.
   contradictions are discovered here.
      integrate with Cons BlockSt and Cons BlockTr.
      make them so strong/expressive, that they can over all of what we need here.
      perhaps, use both Cons to implement a new ConsHistory.
      one which is made of minimal elements only. it only grows,
      when condition-requirements from other places are checked.
      new featureswill only be added, once they are needed.s
 OK   Choice (e.g. on standable vs in standable) will try to first
      make any1 of them concrete and see, whether one can stay on them.
      otherwise, the choice is deferred at a worse case of exp. time.
      practically speaking, ground-check cannot become the feared exp-blowup,
      because even in closed eyes, the current block is visible
      and therefore, the requirement is always given or deferred to the lower block.

5. End of Level: start from t=0 and on each step
   concretize the CondHistory to a unique ConsHistory.
-}


{- STCons == Space-Time-Constraint. used for constraint creation.
  an unassociated key means, that there is no constraint on that field.
  [.] denoted choice. -}
type STCons = Expr (ConsHistoryP,ConsDesc)
{- type denoting partial constructive constraints on history.
constraint on CH_Global, on space-time blocks and it's reason
for being there. -}
data ConsHistoryP = CHP {
     chpspace :: M.Map TimePos (Cons BlockSt, Cons BlockTr)
    ,chpglobal :: CH_GlobalP
  }
  | Unfulfillable
{-
  bool = True => all information in block is fully defined. it can now lead to contradictions.
  bool = False => we have not provided all information, so constrainty may be
    satisfied lateron after adding new elems. -}
type ConsDesc = String
-- result of running an STCons on a constraint history.
type ConsRes = (Maybe ConsHistoryP,ConsDesc)
-- could also use tree.
data Expr a = Leaf a
  | All [Expr a] {- non-empty list! -}
  | Any [Expr a] {- non-empty list! -}
  deriving (Show,Eq,Ord)
;
eLeaf :: a -> Expr a
eLeaf = Leaf
eAll :: [Expr a] -> Expr a
eAll [] = error "eAll: bad argument"
eAll xs = All xs
eAny :: [Expr a] -> Expr a
eAny = Any

foldExpr :: (a -> b) -> ([b] -> b) -> ([b] -> b) -> Expr a -> b
foldExpr leaf allExpr anyExpr = go
  where go e = case e of
          Leaf x  -> leaf x
          All  es -> allExpr (go <$> es)
          Any  es -> anyExpr (go <$> es)
;
alwaysOk :: STCons
alwaysOk = eLeaf (CHP M.empty unknownGlobalP,"ok")

fromBool :: Bool -> ConsDesc -> STCons
fromBool b msg = eLeaf
  (if b then CHP M.empty unknownGlobalP else Unfulfillable,msg)

neverOk :: ConsDesc -> STCons
neverOk msg = eLeaf (Unfulfillable,msg)

orElse :: Expr a -> Expr a -> Expr a
orElse a b = eAny [a,b]

also :: Expr a -> Expr a -> Expr a
also a b = eAll [a,b]

implies :: Bool -> STCons -> STCons
implies b c = if b then c else alwaysOk

instance Monoid STCons where
  mempty = alwaysOk
  mappend = also
;

{- ================= PLAYER-INPUT ===============
precondition:
. when state anticipation is used, then the eyes must be closed during that time
. the player can anticipate both before and after their action
. a transition can also be anticipated (doesn't require closed eyes)
-}
data PlayerInput =
  PAT { eyesOpenedT :: Bool
        ,antcpt0 :: Antcpt BlockSt
        ,paction :: PlayerAction
        ,antcptT :: Antcpt BlockTr
        ,antcpt1 :: Antcpt BlockSt
  } deriving (Typeable)

-- given a focused map of block-states, it returns the successor
-- transition and state assuming, that nothing happens
noActionSucc :: BlockSt -> (BlockTr,BlockSt)
noActionSucc x = let xtr = f x in (xtr, g xtr)
  where f BC{bcps,bcos,bcenv}= BCT {
       bctenv = (bcenv,EnvStays,bcenv)
      ,bctos = MS.map (\o -> (o,NoMotionT,o)) bcos
      ,bctps = MS.map (\p -> (p,Completed NoAction,p)) bcps
    }
        g BCT{bctenv,bctos,bctps} = BC{
       bcenv = thd3 bctenv
      ,bcps  = MS.map thd3 bctps
      ,bcos  = MS.map thd3 bctos
    }
;

noAction :: (Functor f, Functor g) => f (g BlockSt) -> f (g BlockTr)
noAction = fmap (fmap (fst . noActionSucc))
