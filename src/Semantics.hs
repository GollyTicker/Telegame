{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

{- contains information on the semantics of blocks.
  as much as it could be refactored into this file -}

module Semantics (
     successors
    ,predecessors
    ,envOnStandables
    ,envOnStandablesT
    ,envInStandables
    ,envInStandablesT
    ,envPermeables
    ,envPermeablesT
    ,isGrounded
    ,isPermeable
  )
  where

import BaseBlock

successors :: (PlayerT,Player) -> [(Player,PlayerT)]
successors _ = [] -- TODO:
-- e,g. successors (Motion L D,p) -> [(p,Motion U D),(p,CompletedFalling)]

predecessors :: (Player,PlayerT) -> [(PlayerT,Player)]
predecessors _ = [] -- TODO.


envOnStandables = [Just Solid]
envOnStandablesT = [Nothing]

envInStandables = [Just Platform]
envInStandablesT = [Nothing]

envPermeables = Just <$> [Blank,Platform,Switch False,Switch True] ++ (do n' <- [0..4]; h <- [0..n']; return (Door n' h))
envPermeablesT = [Nothing]

{- y-axis is pointed downwards -}
-- isGrounded ensures, that current position is on ground.
-- either through looking at below, or through a platform at same position
isGrounded :: (Show a,Block b) => This b -> TimePos -> a -> STCons
isGrounded ths curr@(t,(x,y)) reason =
  let below = (t,(x,succ y))
  in  in_standable ths curr (show (curr,ths) ++ " needs standable (cause: "++show reason++") in "++ show  curr)
      `orElse` on_standable ths below (show (curr,ths) ++ " needs standable (cause: "++show reason++") on "++ show below)
;

isPermeable :: (Block b,Show a) => This b -> TimePos -> a -> STCons
isPermeable ths curr reason = permeable ths curr (show (curr,ths) ++ " requires permeability (cause: " ++ show reason ++ ")")


