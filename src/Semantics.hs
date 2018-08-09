{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wall -Wno-missing-signatures #-}
#else
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
#endif


{- contains information on the semantics of blocks.
  as much as it could be refactored into this file -}

module Semantics (
    -- todo: export only whats nesessary
     module Semantics
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


