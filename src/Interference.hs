{-# LANGUAGE CPP, TypeFamilies,StandaloneDeriving,DeriveDataTypeable #-}
-- GHC CPP macros: https://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/phases.html#standard-cpp-macros
-- https://guide.aelve.com/haskell/cpp-vww0qd72
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wall -Wno-orphans -Wno-missing-signatures #-}
#else
{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
#endif

module Interference where

import Base
import Data.Data
import ViewBase()
import qualified Data.Set as S
import qualified Data.Map as M
-- import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Data.Monoid
--import Debug.Trace
-- import Control.Applicative

instance Block BlockSt where
  type OpenObs BlockSt = Specific (Space BlockSt)
  type ClosedObs BlockSt = Specific (Pos,BlockSt)
  type Cons BlockSt = BC_Cons
  type Antcpt BlockSt = Space (Maybe BlockSt)
  isState Proxy = True
  on_standable bc = envStandable (bcenv bc)
  in_standable bc = envStandableIn (bcenv bc)
  permeable curr reason bc = fromBool (show curr ++ " requires permeable for " ++ show reason) $ envPermeable (bcenv bc)
  selfconsistent _ = ok -- TODO: implement
  interferesWithBlock = interferesWith
  
  -- find the block where the player is and reduce all to that block.
  reduceToClosed Proxy spo@(Specific _ player _ mp) = spo {sobservations = (pos, mp M.! pos)}
    where pos = case (M.toList . M.filter (any (player==) . bcps) $ mp) of ((p,_):_) -> p ; [] -> error "reduceToClosed: Player not found"
  
  applypwObs p@Proxy sobs fo fc = if peyes (splayer sobs) then fo sobs else fc (reduceToClosed p sobs)
;

instance Block BlockTr where
  type OpenObs BlockTr = Specific (Space BlockTr)
  type ClosedObs BlockTr = Specific (Space BlockTr)
  type Cons BlockTr = BCT_Cons
  type Antcpt BlockTr = Space (Maybe BlockTr)
  isState Proxy = False
  on_standable bct = let (old,change,new) = bctenv bct
                     in  envStandable old && envStandable new && envTStandable change
  in_standable bct = let (old,change,new) = bctenv bct
                     in  envStandableIn old && envStandableIn new && envTStandableIn change
  permeable curr reason bct = let (old,change,new) = bctenv bct
                  in fromBool (show curr ++ "requires remaining permeable for" ++ show reason) $
                      envPermeable old && envPermeable new && envTPermeable change
  selfconsistent _ = ok -- TODO: implement
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


envPermeable :: EnvObj -> Bool
envPermeable Solid = False
envPermeable _ = True

envTStandableIn _ = True

envStandableIn Platform = True
envStandableIn _ = False

envTPermeable :: EnvT -> Bool
envTPermeable _ = True

envStandable :: EnvObj -> Bool
envStandable Solid = True
envStandable _ = False

envTStandable :: EnvT -> Bool
envTStandable _ = True

-- Given a field and predicate for that field, it checks,
-- whether the predicate is satisfied by that field.
mkCCsingle :: TimePos -> (Field -> CondRes) -> ConditionsChecker
mkCCsingle tpos f = CC {ccneeds = S.singleton tpos, ccrun = (:[]) . f . maybe (error "mkCCsingle[fatal]: CC precondition violated") id . M.lookup tpos}

{- y-axis is pointed downwards -}
-- is1 ensures, that current position is on ground.
-- either through looking at below, or through a platform at same position
isGrounded :: Show a => TimePos -> a -> ConditionsChecker
isGrounded curr@(t,(x,y)) reason =
  let below = (t,(x,succ y))
  in  CC { ccneeds = S.fromList [curr,below],
           ccrun   = \mp ->
             (:[])
             $ unknownOkAnd (\foundStd -> if not (getAny foundStd) then show curr ++ " needs standable (cause: "++show reason++")on "++show below ++" or standable in "++ show curr else ok)
             $ (fmap (Any . in_standable) . fst $ mp M.! curr) `mappend` (fmap (Any . on_standable) . fst $ mp M.! below)
      }

isPermeable :: Show a => TimePos -> a -> ConditionsChecker
isPermeable curr reason = mkCCsingle curr (unknownOkAnd (permeable curr reason) . fst)

-- a condition checker specified a set of TimePos
-- it wants to access. it also specifies a function which gets
-- a partial view of the spacetime to conclude whether the condition is satisfied or not
-- ConditionsChecker is a Monoid.
-- CondRes only has a 0-Element, but no binary operation for it.
{- DIFINITION in Base -}
#if __GLASGOW_HASKELL__ >= 840
instance Semigroup ConditionsChecker where -- required by Monoid, since GHC 8.
  (<>) = also
;
#else
#endif
instance Monoid ConditionsChecker where
  mempty = alwaysOk
  mappend = also
;

ok :: CondRes
ok = ""

fromBool :: String -> Bool -> CondRes
fromBool s b = if b then ok else s

isContradiction :: CondRes -> Bool
isContradiction = not . null

unknownOkAnd :: (a -> CondRes) -> Maybe a -> CondRes
unknownOkAnd = maybe ok

alwaysOk :: ConditionsChecker
alwaysOk = CC { ccneeds = S.empty, ccrun = const []}

also :: ConditionsChecker -> ConditionsChecker -> ConditionsChecker
(CC nds runcc) `also` (CC nds' runcc') =
    CC { ccneeds = S.union nds nds',
         ccrun = \mp -> runcc mp ++ runcc' mp}

-- all interferesWith functions check at the current BlockSt.
-- all interferesWithT functions check at the current BlockTr.

interferesWith :: TimePos -> BlockSt -> ConditionsChecker
interferesWith curr bc =
  ( playerInterferesWith curr `foldMap` MS.toList (bcps bc))
  `also` ( phyObjInterferesWith curr `foldMap` MS.toList (bcos bc))
  `also` ( envInterferesWith curr (bcenv bc))
;

hasFutureTrIf :: Show a => TimePos -> a -> (BlockTr -> Bool) -> ConditionsChecker
hasFutureTrIf curr o f = mkCCsingle curr (unknownOkAnd (fromBool (show curr ++" requires future for "++show o) . f) . snd)

hasPastTrIf :: Show a => TimePos -> a -> (BlockTr -> Bool) -> ConditionsChecker
hasPastTrIf (t,pos) o f =
  if t <= 0 then alwaysOk else mkCCsingle (t-1,pos) (unknownOkAnd (fromBool (show (t-1,pos) ++" requires past for "++show o) . f) . snd)

playerInterferesWith :: TimePos -> Player -> ConditionsChecker
playerInterferesWith curr p =
  isGrounded curr p
  `also` isPermeable curr p
  `also` hasFutureTrIf curr p (not . MS.null . MS.filter ((p==) . fst3) . bctps)
  `also` hasPastTrIf   curr p (not . MS.null . MS.filter ((p==) . thd3) . bctps)
;-- we don't require grounded-ness in the transition phase, because that will be handled by the check at the BlockTr.

phyObjInterferesWith :: TimePos -> PhyObj -> ConditionsChecker
phyObjInterferesWith curr o =
  isGrounded curr o
  `also` isPermeable curr o
  `also` hasFutureTrIf curr o existsInT
  `also` hasPastTrIf   curr o existsInT
  where
    existsInT bct = maybe False (all (validObjAction o)) $ M.lookup o (bctos bct)
-- physical objects are only checked, when they are on ground.
-- they are not checked, when they are in doors or in player's inventories.
;

validObjAction (TOrb _ _) _act = True
validObjAction _           act = act `notElem` [TPsend,TPget]

envInterferesWith :: TimePos -> EnvObj -> ConditionsChecker
envInterferesWith curr env =
  {- is grounded -}
  (if (case env of Door _ _ -> True; Switch _ -> True; _ -> False) then isGrounded curr env else alwaysOk)
  `also` hasFutureTrIf curr env ((env==) . fst3 . bctenv)
  `also` hasPastTrIf   curr env ((env==) . thd3 . bctenv)

interferesWithT :: TimePos -> BlockTr -> ConditionsChecker
interferesWithT _ _ = alwaysOk {- TODO: implement this -}

-- a function crucial for concreteHistory.
-- given a set of conditions to be satisfied,
-- it searches for the simplest BlockSt that satisfies it.
inferMinimal :: BC_Cons -> MayFail BlockSt
inferMinimal _ = failing "TODO: implement inferMinimal"
-- TODO: inferMinimalT
