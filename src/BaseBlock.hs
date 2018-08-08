{-# LANGUAGE TupleSections,NamedFieldPuns,FlexibleContexts,TypeFamilies,DeriveFunctor #-}
{-# OPTIONS_GHC -Wall #-}

module BaseBlock (
     module Base
    ,Block(..)
    ,BlockSt(..)
    ,BlockTr(..)
    ,fst3,snd3,thd3
    ,BC_Cons(..)
    ,BCT_Cons(..)
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
    ,ConsHistory(..)
    ,CondRes
    ,ConditionsChecker(..)
    ,PlayerInput(..)
  ) where

import Base
import Data.Data
import qualified Data.Set as S
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import qualified Data.Map as M
import qualified Data.Maybe as Maybe


{- general class to put together BlockTr and BlockSt. instantiations in Inference.hs -}
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
  reduceToClosed :: Proxy a -> OpenObs a -> ClosedObs a
  applypwObs :: Proxy a -> PWorld a -> (OpenObs a -> b) -> (ClosedObs a -> b) -> b
;

{- contents of a state block. multiset, because the same obj/plyr can occur identically many times -}
data BlockSt = BC { bcps :: MultiSet Player, bcos :: MultiSet PhyObj, bcenv :: Env }
  deriving (Eq,Ord)
;
data BlockTr = BCT {
    bctenv  :: (Env,EnvT,Env) -- old env, environment change, new env (possibly same)
   ,bctos   :: M.Map PhyObj (MultiSet PhyObjT) -- object motion. since objects can be multiple, each object is identified with a multiset of motions
   ,bctps   :: MultiSet (Player,PlayerT,Player) -- player before -> action -> player after
  }
  deriving (Eq,Ord)

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a
snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b
thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c

{- explicit minimality constraint representation of BlockSt/BlockTr -}
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
{-
IMPORTANT CONDITIONS:
An unknown BlockSt has to be uniquely determinable from
fully known neighboring BlockContentTs and distant blocks it interferes with (e.g. jump or teleport).
(unless contradiction of course).
same holds for BlockTr.
=> using explicit constraints. BC_Cons and BCT_Cons
-}

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
type MayContra a = Either [CondRes] a

runMayContra :: ([CondRes] -> a) -> (b -> a) -> MayContra b -> a
runMayContra = either

failing :: String -> MayContra a
failing = Left . (:[])

success :: a -> MayContra a
success = Right

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right 



{- =================== GAMESTATE =================== -}
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
type CH_Global = M.Map Char TeleportInfo -- no in map is eqivalient to unknown.
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


{- ===== CONDITIONS CHECKING & EXAMPLE GENERATION ===== -}
type CondRes = String -- "" means satisfied. otherwise contradiction with description.
-- TODO:
{-

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
      Choice (e.g. on standable vs in standable) will try to first
      make any1 of them concrete and see, whether one can stay on them.
      otherwise, the choice is deferred at a worse case of exp. time.
      practically speaking, ground-check cannot become the feared exp-blowup,
      because even in closed eyes, the current block is visible
      and therefore, the requirement is always given or deferred to the lower block.

5. End of Level: start from t=0 and on each step
   concretize the CondHistory to a unique ConsHistory.
-}
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
  where f (BC ps os env) = BCT {
       bctenv = (env,EnvStays,env)
      ,bctos = M.fromListWith MS.union $ map (,MS.singleton NoMotionT) $ MS.toList os
      ,bctps = MS.map (\p -> (p,Completed NoAction,p)) ps
    }
        g BCT{bctenv,bctos,bctps} = BC{
       bcenv = thd3 bctenv
      ,bcps  = MS.map thd3 bctps
      ,bcos  = MS.unions . M.elems
        . M.mapWithKey (\o -> catMaybes . MS.map (afterPhyObjT o)) $ bctos
    }
;
catMaybes ::Ord a => MultiSet (Maybe a) -> MultiSet a
catMaybes = MS.fromList . Maybe.catMaybes . MS.toList

noAction :: (Functor f, Functor g) => f (g BlockSt) -> f (g BlockTr)
noAction = fmap (fmap (fst . noActionSucc))

