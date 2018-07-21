
{-# OPTIONS_GHC -Wall -Wno-missing-signatures #-}

module Interference where

import Base
import View()
import qualified Data.Set as S
import qualified Data.Map as M
-- import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Data.Monoid

class Block a where
  on_standable :: a -> Bool
  in_standable :: a -> Bool
  permeable :: TimePos -> a -> CondRes
  selfconsistent :: a -> CondRes
  interferesWithBlock :: TimePos -> a -> ConditionsChecker
;

instance Block BlockContent where
  on_standable bc = envStandable (bcenv bc)
  in_standable bc = envStandableIn (bcenv bc)
  permeable curr bc = fromBool (show curr ++ " requires permeable") $ envPermeable (bcenv bc)
  selfconsistent _ = ok -- TODO: implement
  interferesWithBlock = interferesWith
;

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

instance Block BlockContentT where
  on_standable bct = let (old,change,new) = bctenv bct
                     in  envStandable old && envStandable new && envTStandable change
  in_standable bct = let (old,change,new) = bctenv bct
                     in  envStandableIn old && envStandableIn new && envTStandableIn change
  permeable curr bct = let (old,change,new) = bctenv bct
                  in fromBool (show curr ++ "requires remaining permeable") $
                      envPermeable old && envPermeable new && envTPermeable change
  selfconsistent _ = ok -- TODO: implement
  interferesWithBlock = interferesWithT
;



mkCCsingle :: TimePos -> (Field -> CondRes) -> ConditionsChecker
mkCCsingle tpos f = CC {ccneeds = S.singleton tpos, ccrun = (:[]) . f . maybe (error "mkCCsingle[fatal]: CC precondition violated") id . (M.!? tpos)}

{- y-axis is pointed downwards -}
-- isGrounded ensures, that current position is on ground.
-- either through looking at below, or through a platform at same position
isGrounded :: TimePos -> ConditionsChecker
isGrounded curr@(t,(x,y)) =
  let below = (t,(x,succ y))
  in  CC { ccneeds = S.fromList [curr,below],
           ccrun   = \mp ->
             (:[])
             $ unknownOkAnd (\foundStd -> if not (getAny foundStd) then show curr ++ " needs standable on "++show below ++" or standable in "++ show curr else ok)
             $ (fmap (Any . in_standable) . fst $ mp M.! curr) `mappend` (fmap (Any . on_standable) . fst $ mp M.! below)
      }

isPermeable :: TimePos -> ConditionsChecker
isPermeable curr = mkCCsingle curr (unknownOkAnd (permeable curr) . fst)

-- a condition checker specified a set of TimePos
-- it wants to access. it also specifies a function which gets
-- a partial view of the spacetime to conclude whether the condition is satisfied or not
-- ConditionsChecker is a Monoid.
-- CondRes only has a 0-Element, but no binary operation for it.
type CondRes = String -- "" means satisfied. otherwise contradiction with description.
ok :: CondRes
ok = ""
data ConditionsChecker =
  CC {
    ccneeds :: S.Set TimePos
   ,ccrun :: M.Map TimePos Field -> [CondRes] -- PRECONDITION: ccrun is called on a map,
                                              -- which is defined for all keys in ccneeds!
 -- perhaps add output method to generate contradiction results?
 -- returns a CondRes for every check.
  }
;
instance Semigroup ConditionsChecker where -- required by Monoid
  (<>) = also
;
instance Monoid ConditionsChecker where
  mempty = alwaysOk
  mappend = also
;

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

interferesWith :: TimePos -> BlockContent -> ConditionsChecker
interferesWith curr bc =
  ( playerInterferesWith curr `foldMap` MS.toList (bcps bc))
  `also` ( phyObjInterferesWith curr `foldMap` MS.toList (bcos bc))
  `also` ( envInterferesWith curr (bcenv bc))
;

playerInterferesWith :: TimePos -> Player -> ConditionsChecker
playerInterferesWith curr@(t,pos) p =
  isGrounded curr `also` isPermeable curr
  `also` mkCCsingle (t,pos) (unknownOkAnd (fromBool (show (t,pos) ++" requires future for "++show p) . (not . MS.null . MS.filter ((p==) . fst3) . bctps)) . snd)
  `also` if t<=0 then alwaysOk else mkCCsingle (t-1,pos) (unknownOkAnd (fromBool (show (t-1,pos) ++" requires past for "++show p) . (not . MS.null . MS.filter ((p==) . thd3) . bctps)) . snd)
  {- player has a past, unless t=0 -}
;

phyObjInterferesWith :: TimePos -> PhyObj -> ConditionsChecker
phyObjInterferesWith _ _ = alwaysOk {- TODO: implement -}

envInterferesWith :: TimePos -> EnvObj -> ConditionsChecker
envInterferesWith _ _ = alwaysOk {- TODO: implement -}

interferesWithT :: TimePos -> BlockContentT -> ConditionsChecker
interferesWithT _ _ = alwaysOk {- TODO: implement this -}

-- a function crucial for concreteHistory.
-- given a set of conditions to be satisfied,
-- it searches for the simplest BlockContent that satisfies it.
inferMinimal :: BC_Cons -> MayFail BlockContent
inferMinimal _ = failing "TODO: implement inferMinimal"
-- TODO: inferMinimalT
