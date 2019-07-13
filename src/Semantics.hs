{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

{- contains information on the semantics of blocks.
  as much as it could be refactored into this file -}

module Semantics (
     movingSuccessors
    ,movingPredecessors
    ,envOnStandables
    ,envOnStandablesT
    ,envInStandables
    ,envInStandablesT
    ,envPermeables
    ,envPermeablesT
    ,isGrounded
    ,isPermeable
    ,phyObjTo
    ,phyObjFrom
  )
  where

import BaseBlock
import ViewBase()

movingSuccessors :: (PlayerT,Player) -> [(Player,PlayerT)]
movingSuccessors (pt,p) = zip [p,p] $ case pt of
  Initiated MoveL -> [Motion R D, Completed MoveL]
  Initiated MoveR -> [Motion L D, Completed MoveR]
  Initiated JumpU  -> [Motion D D, Completed JumpU]
  Initiated JumpUL -> [Motion D L]
  Initiated JumpUR -> [Motion D R]
  Motion L D -> [Motion U D, CompletedFalling]
  Motion D R -> [Motion L D, Completed JumpUR]
  Motion R D -> [Motion U D, CompletedFalling]
  Motion D L -> [Motion R D, Completed JumpUL]
  Motion D D -> [Motion U D, CompletedFalling]
  Motion U D -> [Motion U D, CompletedFalling]
  Motion _ _ -> error $ "success: Motion invalid"
  _ -> []

movingPredecessors :: (Player,PlayerT) -> [(PlayerT,Player)]
movingPredecessors (p,pt) = flip zip [p,p] $ case pt of
  Initiated _ -> []
  Motion L D -> [Initiated MoveR ]
  Motion D R -> [Initiated JumpUR]
  Motion R D -> [Initiated MoveL ]
  Motion D L -> [Initiated JumpUL]
  Motion D D -> [Initiated JumpU ]
  Motion U D -> [Motion L D, Motion U D, Motion R D]
  Motion _ _ -> error $ "success: Motiong invalid"
  Completed MoveL  -> [Initiated MoveL]
  Completed MoveR  -> [Initiated MoveR]
  Completed JumpU  -> [Initiated MoveR]
  Completed JumpUL -> [Motion D L     ]
  Completed JumpUR -> [Motion D R     ]
  CompletedFalling -> [Motion L D, Motion U D, Motion R D]
  Completed _      -> []



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


{- player constraints T -}


{- phyobj constraint T -}

phyObjTo :: Dir -> [PhyObjT]
phyObjTo U = []
phyObjTo d = map (uncurry MotionT)
  . filter ((==d).snd)
  $ zip [L,L,U,R,R] [D,R,D,D,L]


phyObjFrom :: Dir -> [PhyObjT]
phyObjFrom D = []
phyObjFrom d = LandFrom d:
  (map (uncurry MotionT)
    . filter ((==d).fst)
    $ zip [L,L,U,R,R] [D,R,D,D,L])
