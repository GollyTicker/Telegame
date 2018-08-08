{-# LANGUAGE CPP, TypeFamilies,StandaloneDeriving,DeriveDataTypeable,FlexibleContexts #-}
-- GHC CPP macros: https://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/phases.html#standard-cpp-macros
-- https://guide.aelve.com/haskell/cpp-vww0qd72
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wall -Wno-orphans -Wno-missing-signatures #-}
#else
{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
#endif

module Interference(
     module BaseBlock
    ,interferesWith
    ,interferesWithT
    ,inferMinimal
    ,inferMinimalT
    ,adjustGlobalInfo
    ,isContradiction
    
    ,This(..)
    ,is_a
    ,ok
    ,notok
    ,Cons(..)
  )
  where

import BaseBlock
import Data.Data
import Control.Monad (foldM)
import ViewBase()
import qualified Data.Set as S
import qualified Data.Map as M
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Data.Monoid
-- import Debug.Trace
-- import Control.Applicative

instance Block BlockSt where
  type OpenObs BlockSt = Specific (Space BlockSt)
  type ClosedObs BlockSt = Specific (Pos,BlockSt)
  newtype Cons BlockSt = BCC BlockSt deriving (Ord,Eq)
    -- OneP Player | OneO PhyObj | Bgrd Env
    -- => e.g. OneP p1 & OneP p1 & OneP p2 & Bgrd (Door 0 0) <=>
    -- BSCons MultiSet(p1,p1,p2) + Door 0 0 ==> ".P1. .P1. .P2. D00"
  type Antcpt BlockSt = Space (Maybe BlockSt)
  type Other BlockSt = BlockTr
  data This BlockSt = St
  this = St
  getter St = fst
  on_standable bc = envStandable (bcenv bc)
  in_standable bc = envStandableIn (bcenv bc)
  permeable curr reason bc = fromBool (show (curr,St) ++ " requires permeable for " ++ show reason) $ envPermeable (bcenv bc)
  selfconsistent _ = todo -- TODO: implement. required counting ability => BC_Cons and BCT_Cons
  interferesWithBlock = interferesWith
  
  -- find the block where the player is and reduce all to that block.
  reduceToClosed Proxy spo@(Specific _ player _ mp) = spo {sobservations = (pos, mp M.! pos)}
    where pos = case (M.toList . M.filter (any (player==) . bcps) $ mp) of ((p,_):_) -> p ; [] -> error "reduceToClosed: Player not found"
  
  applypwObs p@Proxy sobs fo fc = if peyes (splayer sobs) then fo sobs else fc (reduceToClosed p sobs)
;

instance Block BlockTr where
  type OpenObs BlockTr = Specific (Space BlockTr)
  type ClosedObs BlockTr = Specific (Space BlockTr)
  data Cons BlockTr =
      BCTC {
        bctcInit :: Maybe Env
       ,bctcEnd  :: Maybe Env
       ,bctcToFutrP :: MultiSet Player
       ,bctcToFutrO :: MultiSet PhyObj
       ,bctcFromPastP :: MultiSet Player
       ,bctcFromPastO :: MultiSet PhyObj
    } deriving (Eq,Ord)
  type Antcpt BlockTr = Space (Maybe BlockTr)
  type Other BlockTr = BlockSt
  data This BlockTr = Tr
  this = Tr
  getter Tr = snd
  on_standable bct = let (old,change,new) = bctenv bct
                     in  envStandable old && envStandable new && envTStandable change
  in_standable bct = let (old,change,new) = bctenv bct
                     in  envStandableIn old && envStandableIn new && envTStandableIn change
  permeable curr reason bct = let (old,change,new) = bctenv bct
                  in fromBool (show (curr,Tr) ++ "requires remaining permeable for" ++ show reason) $
                      envPermeable old && envPermeable new && envTPermeable change
  selfconsistent _ = todo -- TODO: implement
  interferesWithBlock = interferesWithT
  
  -- get all block where the player is identified
  reduceToClosed Proxy spo@(Specific _ plyr _ mp) =   spo { sobservations = M.filter (any (\(p1,_,p2) -> p1 == plyr || p2 == plyr) . MS.toList . bctps) mp}
  
  applypwObs p@Proxy sobs fo fc = if peyes (splayer sobs) then fo sobs else fc (reduceToClosed p sobs)
;

deriving instance Data Player
deriving instance Typeable Player
deriving instance Data PlayerAction
deriving instance Typeable PlayerAction
deriving instance Data PlayerT
deriving instance Typeable PlayerT
deriving instance Data ConsHistory
deriving instance Typeable ConsHistory
deriving instance Data a => Data (Specific a)
deriving instance Typeable a => Typeable (Specific a)
deriving instance Data BlockSt
deriving instance Typeable BlockSt
deriving instance Data BlockTr
deriving instance Typeable BlockTr
deriving instance Data GameState
deriving instance Typeable GameState

is_a :: (Block b,Block c) => This b -> This c -> Bool
ths `is_a` oth = typeOf ths == typeOf oth

envPermeable :: Env -> Bool
envPermeable Solid = False
envPermeable _ = True

envTStandableIn _ = True

envStandableIn Platform = True
envStandableIn _ = False

envTPermeable :: EnvT -> Bool
envTPermeable _ = True

envStandable :: Env -> Bool
envStandable Solid = True
envStandable _ = False

envTStandable :: EnvT -> Bool
envTStandable _ = True


mkGlobalCC :: Block b => (CH_Global -> ConsRes b) -> Constraint b
mkGlobalCC f = C { runCons = \_ g -> f g, cneeds = S.empty }

mkSimpleCCwithGlobal :: Block b =>
  TimePos -> (Field -> CH_Global -> ConsRes b) -> Constraint b
mkSimpleCCwithGlobal tpos f = C {
  runCons = \mp g -> flip f g . maybe (error "mkSimpleCC[fatal]: CC precondition violated") id . M.lookup tpos $ mp
 ,cneeds = S.singleton tpos }
  
mkSimpleCC :: Block b =>  TimePos -> (Field -> ConsRes b) -> Constraint b
mkSimpleCC tpos f = C {
   runCons = \mp _g -> f . maybe (error "mkSimpleCC[fatal]: CC precondition violated") id . M.lookup tpos $ mp
  ,cneeds = S.singleton tpos
}
{- y-axis is pointed downwards -}
-- is1 ensures, that current position is on ground.
-- either through looking at below, or through a platform at same position
isGrounded :: (Show a,Block b) => This b -> TimePos -> a -> Constraint b
isGrounded ths curr@(t,(x,y)) reason = todo {-
  let below = (t,(x,succ y))
  in  CC { ccneeds = S.fromList [curr,below],
           ccrun   = \mp _g ->
             (:[])
             . (unknownOkAnd (\foundStd -> if not (getAny foundStd) then show (curr,ths) ++ " needs standable (cause: "++show reason++") on "++show below ++" or standable in "++ show curr else ok))
             $ (fmap (Any . in_standable) . getter ths $ mp M.! curr) `mappend` (fmap (Any . on_standable) . getter ths $ mp M.! below)
      }
;-}

isPermeable :: (Block b,Show a) => This b -> TimePos -> a -> Constraint b
isPermeable ths curr reason = todo --mkSimpleCC curr $ unknownOkAnd (permeable curr reason) . getter ths

-- a condition checker specified a set of TimePos
-- it wants to access. it also specifies a function which gets
-- a partial view of the spacetime to conclude whether the condition is satisfied or not
-- Constraint b is a Monoid.
-- CondRes only has a 0-Element, but no binary operation for it.
{- DIFINITION in Base -}
#if __GLASGOW_HASKELL__ >= 840
instance Semigroup (Constraint b) where -- required by Monoid, since GHC 8.
  (<>) = also
;
#else
#endif
instance Block b => Monoid (Constraint b) where
  mempty = alwaysOk
  mappend = also
;

{- connectives and neutral-elem for building constraint from others. -}
{-cand :: Block b => Cons b -> Cons b -> Cons b
cand = todo
cor  :: Block b => Cons b -> Cons b -> Cons b
cor  = todo
cleast :: Block b => Cons b
cleast = todo
-}

ok :: Block b => Cons b -> ConsRes b
ok = Right

notok :: ConsFail -> ConsRes b
notok = Left

orElse :: Block b => Constraint b -> Constraint b -> Constraint b
orElse = todo

satisfies :: Block b => b -> Cons b -> Maybe ConsFail
satisfies = todo

fromBool :: Block b => String -> Bool -> ConsRes b
fromBool s b = if b then ok todo else Left s

isContradiction :: Block b => ConsRes b -> Bool
isContradiction = not . null

unknownOkAnd :: Block b => (a -> ConsRes b) -> Maybe a -> ConsRes b
unknownOkAnd = maybe (ok todo)

alwaysOk :: Block b => Constraint b
alwaysOk = todo -- C { ccrun = \_ _ -> [], cneeds = S.empty}

also :: Block b => Constraint b -> Constraint b -> Constraint b
(C run nds) `also` (C run' nds') = todo {-
    CC { ccneeds = S.union nds nds',
         ccrun = \mp g-> runcc mp g ++ runcc' mp g} -}

-- all interferesWith functions check at the current BlockSt.
-- all interferesWithT functions check at the current BlockTr.

interferesWith :: TimePos -> BlockSt -> Constraint BlockSt
interferesWith curr bc =
  ( playerInterferesWith curr `foldMap` MS.toList (bcps bc))
  `also` ( phyObjInterferesWith curr `foldMap` MS.toList (bcos bc))
  `also` ( envInterferesWith curr (bcenv bc))
;

interferesWithT :: TimePos -> BlockTr -> Constraint BlockTr
interferesWithT curr bct =
  ( playerInterferesWithT curr `foldMap` MS.toList (bctps bct) )
  `also` ( (\(o,ot) -> phyObjInterferesWithT curr o `foldMap` ot) `foldMap` M.toList (bctos bct))
  `also` ( envInterferesWithT curr (bctenv bct))
;

playerInterferesWithT :: TimePos -> (Player,PlayerT,Player) -> Constraint BlockTr
playerInterferesWithT curr p = todo {-
  let needsGroundCheck = runpat (const True) (\_ _ -> False) (const True) True (snd3 p)
      {- case (Completed pa): currently, all completed actions require a grounded place as dest. -}
      {- physical movement: movingNext = Nothing, if player doesn't move out of current block.
         Just Dir, if the player continues in direction Dir. time-inverse with Next<->From. teleport is not included here -}
      movingNext   = runpat toDirpa (\_ y->Just y) (const Nothing) Nothing (snd3 p)
      nextReqStr d = show (curr,Tr) ++ " requires a continuation of movement "     ++ show (snd3 p) ++ " to " ++ show (applyDir d curr,Tr)
      movingFrom   = runpat (const Nothing) (\x _->Just x) fromDirpa (Just U) (snd3 p)
      fromReqStr d = show (curr,Tr) ++ " requires a preceding action to continue " ++ show (snd3 p) ++ " from " ++ show (applyDir d curr,Tr)
      leavesPuzzle = case snd3 p of
        (Completed (UseEnvMult es)) -> case es of [] -> False; _non_empty -> last es == TraverseDoor
        _ -> False
      -- globalMismatch :: PlayerAction -> PlayerAction -> ConsRes ?
      -- globalMismatch tpl tpg = show (curr,Tr) ++ " contains " ++ show tpl ++ " which should be consistent to global information " ++ show tpg
  in (if needsGroundCheck then isGrounded Tr curr p else alwaysOk)
     `also` isPermeable Tr curr p
     `also` (case snd3 p of {- teleport cases (todo) -}
        Initiated _tp@(Teleport {}) -> alwaysOk
          -- mkGlobalCC (\g -> unknownOkAnd (\tp' -> fromBool (globalMismatch tp tp') (tp==tp')) $ M.lookup (tpch tp) g)
          -- this actually belongs to the self-consistency tests.
          -- why not simply do self-consistency check here as well?
          -- todo: a check for landing on the other side is not nesessary, as each will know,
          -- that it is itself consistent with global information when the check runs.
          -- or perhaps, we need idenpendent consistency checks for the global-infos.
          -- because, if the partner tp-palce has no tp-action, this would not be recognized currently.
        Completed _tp@(Teleport {}) -> alwaysOk
          -- mkGlobalCC (\g -> unknownOkAnd (\tp' -> fromBool (globalMismatch tp tp') (tp==tp')) $ M.lookup (tpch tp) g)
        _ {- non-teleport cases -}  ->
          (if leavesPuzzle then alwaysOk else case movingNext of
              Nothing -> hasFutureIf Tr St curr (thd3 p) (any ((thd3 p)==) . bcps)
              Just d  -> mkSimpleCC (applyDir d curr) $ unknownOkAnd (fromBool (nextReqStr d) . any (p `canBePredOf`) . bctps) . getter Tr
          ) `also` case movingFrom of
              Nothing -> hasPastIf Tr St curr (fst3 p) (any ((fst3 p)==) . bcps);
              Just d  -> mkSimpleCC (applyDir d curr) $ unknownOkAnd (fromBool (fromReqStr d) . any (`canBePredOf` p) . bctps) . getter Tr
      )-}
{- todo: higher-prototype: check, what all these items do.
  handle teleportation processes by using a Reader which holds information
  on current global things like tp[char], switch etc.-}

canBePredOf :: (Player,PlayerT,Player) -> (Player,PlayerT,Player) -> Bool
(_,_act1,p1) `canBePredOf ` (p2,_act2,_) = p1 == p2 {- todo: -}

hasFutureIf :: (Show a,Block b,Block c) => This b -> This c -> TimePos -> a -> (c -> Bool) -> Constraint b
hasFutureIf ths oth (t,pos) o f =
  let dest = if ths `is_a` St then (t,pos) else (t+1,pos)
  in  mkSimpleCC dest $ unknownOkAnd (fromBool (show ((t,pos),ths) ++" requires future for "++show o ++ " at " ++ show (dest,oth)) . f) . getter oth
;

hasPastIf :: (Show a,Block b,Block c) => This b -> This c -> TimePos -> a -> (c -> Bool) -> Constraint b
hasPastIf ths oth (t,pos) o f =
  let (dest,atBoundary) = if ths `is_a` St then ((t-1,pos),t <= 0) else ((t,pos),False)
  in  if atBoundary then alwaysOk
      else mkSimpleCC dest $ unknownOkAnd (fromBool (show ((t,pos),ths) ++" requires past for "++show o ++" at " ++ show (dest,oth)) . f) . getter oth
;

phyObjInterferesWithT :: TimePos -> PhyObj -> PhyObjT -> Constraint BlockTr
phyObjInterferesWithT _ _ _ = alwaysOk

envInterferesWithT :: TimePos -> (Env,EnvT,Env) -> Constraint BlockTr
envInterferesWithT _ _ = alwaysOk

playerInterferesWith :: TimePos -> Player -> Constraint BlockSt
playerInterferesWith curr p =
  isGrounded St curr p
  `also` isPermeable St curr p
  `also` hasFutureIf St Tr curr p (not . MS.null . MS.filter ((p==) . fst3) . bctps)
  `also` hasPastIf   St Tr curr p (not . MS.null . MS.filter ((p==) . thd3) . bctps)
;-- we don't require grounded-ness in the transition phase, because that will be handled by the check at the BlockTr.

phyObjInterferesWith :: TimePos -> PhyObj -> Constraint BlockSt
phyObjInterferesWith curr o =
  isGrounded St curr o
  `also` isPermeable St curr o
  `also` hasFutureIf St Tr curr o existsInT
  `also` hasPastIf   St Tr curr o existsInT
  where
    existsInT bct = maybe False (all (validObjAction o)) $ M.lookup o (bctos bct)
-- physical objects are only checked, when they are on ground.
-- they are not checked, when they are in doors or in player's inventories.
;

validObjAction (TOrb _ _) _act = True
validObjAction _           act = act `notElem` [TPsend,TPget]

envInterferesWith :: TimePos -> Env -> Constraint BlockSt
envInterferesWith curr env =
  {- is grounded -}
  (if (case env of Door _ _ -> True; Switch _ -> True; _ -> False) then isGrounded St curr env else alwaysOk)
  `also` hasFutureIf St Tr curr env ((env==) . fst3 . bctenv)
  `also` hasPastIf   St Tr curr env ((env==) . thd3 . bctenv)
;

adjustGlobalInfo :: BlockTr -> ConsHistory -> MayContra ConsHistory
adjustGlobalInfo bct ch0 = foldM f ch0 (bctps bct) where
  insertable t ch = maybe True (t==) $ M.lookup (tpch t) (chglobal ch)
  inserttp   t ch = ch {chglobal = M.insert (tpch t) t (chglobal ch)}
  failStr    t ch = "Teleport observation "++show t++" contradicts established global-information " ++ show (chglobal ch)
  f ch (_,Completed (tp@Teleport{}),_) = if insertable tp ch then success (inserttp tp ch) else failing $ failStr tp ch
  f ch (_,Initiated (tp@Teleport{}),_) = if insertable tp ch then success (inserttp tp ch) else failing $ failStr tp ch
  f ch _ = success ch
;

-- a function crucial for concreteHistory.
-- given a set of conditions to be satisfied,
-- it searches for the simplest BlockSt that satisfies it.
inferMinimal :: Cons BlockSt -> MayContra BlockSt
inferMinimal _ = failing "TODO: implement inferMinimal"

inferMinimalT :: Cons BlockSt -> MayContra BlockSt
inferMinimalT _ = failing "TODO: implement inferMinimalT"

