{-# LANGUAGE RankNTypes, ImpredicativeTypes, ExistentialQuantification, FlexibleContexts #-}

module Maps
  where

import Base
import View
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import Control.Monad

map1_Just_in_Reach = Map { size =(7,'D'),
  env = fromNestedList
    [
       ["","","_","","","","D00",""]
      ,["S","","","","","S","S","S"]
      ,["S","S","","","","S","S","S"]
      ,["S","S","S","S","S","S","S","S"]
    ]
}
map1_Just_in_Reach_init :: GameState
map1_Just_in_Reach_init =
  let spec = Specific 0 "P" 0 $ OpenObs map1_Just_in_Reach
        (S.fromList $ map (\i -> ((5,'A'), TOrb ('x',i))) [0,1])
        (S.singleton ((0,'A'),Player "P" 0 True S.empty))
      spec2 = Specific 1 "P" 1 $ OpenObs map1_Just_in_Reach
        (S.fromList $ map (\i -> ((6,'A'), TOrb ('x',i))) [0,1])
        (S.singleton ((6,'A'),Player "P" 1 False (S.singleton Key)))
      closedobs2 = Specific 1 "P" 1 $ ClosedObs (6,'A') (Door 0 0)
                    (S.fromList $ map (\i -> TOrb ('x',i)) [0,1])
                    (S.singleton $ Player "P" 1 False (S.singleton Key))
  in GS (S.fromList [PSO spec spec,PSC closedobs2 spec2]) Seq.empty
;

main = do
  mapM_ (print >=> const (putStrLn "")) $ initialGS map1_Just_in_Reach_init
  let pa = Player "P" 1 False (S.fromList [TOrb ('x',1),Key])
  print $ pa
  putStrLn "parses correctly:"
  print $ pa == (read . show) pa