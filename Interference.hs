
{-# OPTIONS_GHC -Wall #-}

module Interference where

import Base
--import qualified Data.Set as S
--import qualified Data.Map as M
-- import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS

class Block a where
  standable :: a -> CondRes
  permeable :: a -> CondRes
;

instance Block BlockContent where
  standable bc = fromBool "required standable" $ envStandable (bcenv bc)
  permeable bc = fromBool "required permeable" $ envPermeable (bcenv bc)
;

envPermeable :: EnvObj -> Bool
envPermeable Solid = False
envPermeable _ = True

envTPermeable :: EnvT -> Bool
envTPermeable _ = True

envStandable :: EnvObj -> Bool
envStandable Solid = True
envStandable _ = False

envTStandable :: EnvT -> Bool
envTStandable _ = True

instance Block BlockContentT where
  standable bct = let (old,change,new) = bctenv bct
                  in fromBool "required remaining standable" $
                      envStandable old && envStandable new && envTStandable change
  permeable bct = let (old,change,new) = bctenv bct
                  in fromBool "required remaining permeable" $
                      envPermeable old && envPermeable new && envTPermeable change
;


{- y-axis is pointed downwards -}
isGrounded :: TimePos -> ConditionsChecker
isGrounded (t,(x,y)) = [(t,(x,succ y),unknownOkAnd standable,unknownOkAnd standable)]

isPermeable :: TimePos -> ConditionsChecker
isPermeable (t,pos) = [(t,pos,unknownOkAnd permeable,unknownOkAnd permeable)]

hasPlayerFuture :: TimePos -> Player -> ConditionsChecker
hasPlayerFuture = undefined

-- a condition checker is a list of tuples - which is defined for each BlockContent, if it were at TimePos (argument of interferes with)
-- each tuple stands for a condition check at the element (time,pos) in the history.
-- the two functions describe conditions which are to hold
-- for the state and transition for the time and pos.
-- The Maybe corresponds to an unknown block. usually, this is ok.
-- if the String is empty, then the conditions holds. if it's non-empty, then it describes the problem
-- TODO: change ConditionsChecker. it doesn't allow for joint-space condition checking.
-- extend via Tuple to a stronger full consHistory checker?
-- this part is going to be rewritten anyways, because of
-- the constraint-solving in concreteHistory
type CondRes = String -- "" means satisfied. otherwise contradiction with description.
type ConditionsChecker = [(Time,Pos,(Maybe BlockContent -> CondRes),(Maybe BlockContentT -> CondRes))]

fromBool :: String -> Bool -> CondRes
fromBool s b = if b then "" else s

unknownOkAnd :: (a -> String) -> Maybe a -> CondRes
unknownOkAnd = maybe mempty
-- ConditionsChecker as a monoid

also :: ConditionsChecker -> ConditionsChecker -> ConditionsChecker
also = mappend

interferesWith :: TimePos -> BlockContent -> ConditionsChecker
interferesWith tpos bc =
  ( MS.toList (bcps bc) >>= playerInterferesWith tpos )
  ++ ( MS.toList (bcos bc) >>= phyObjInterferesWith tpos )
  ++ ( envInterferesWith tpos (bcenv bc))
;

playerInterferesWith :: TimePos -> Player -> ConditionsChecker
playerInterferesWith curr p =
  isGrounded curr  {- player has a ground -}
  `also` isPermeable curr {- player is not hidden by env -}
  `also` hasPlayerFuture curr p {- player has a future -}
  {- player has a past, unless t=0 -}
;

phyObjInterferesWith :: TimePos -> PhyObj -> ConditionsChecker
phyObjInterferesWith _ _ = [] {- TODO: implement -}

envInterferesWith :: TimePos -> EnvObj -> ConditionsChecker
envInterferesWith _ _ = [] {- TODO: implement -}

interferesWithT :: TimePos -> BlockContentT -> ConditionsChecker
interferesWithT _ _ = [] {- TODO: implement this -}


-- a function crucial for concreteHistory.
-- given a set of conditions to be satisfied,
-- it searches for the simplest BlockContent that satisfies it.
inferMinimal :: BC_Cons -> MayFail BlockContent
inferMinimal _ = failing "TODO: implement inferMinimal"
-- TODO: inferMinimalT
