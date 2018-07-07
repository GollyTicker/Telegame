{-# LANGUAGE RankNTypes, ImpredicativeTypes, ExistentialQuantification, FlexibleContexts #-}

module Maps
  where

import Base
import View
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import Control.Monad



map1_P0 = Specific 0 "P" 0 $ Sized { size = (7,'D'),
  mapping = fromNestedList
    [
      [".P0.","","_","","","tx1 tx0","D00",""]
     ,["S","","","","","S","S","S"]
     ,["S","S","","","","S","S","S"]
     ,["S","S","S","S","S","S","S","S"]
    ]
}

map1_P1 = Specific 1 "P" 1 $ Sized { size = (7,'D'),
  mapping = fromNestedList
    [
      ["","","_","","","",".<P1>(k). tx0 tx1 D00",""]
     ,["S","","","","","S","S","S"]
     ,["S","S","","","","S","S","S"]
     ,["S","S","S","S","S","S","S","S"]
    ]
}

main = do
  mapM_ (print >=> const (putStrLn "")) $ [map1_P0, map1_P1]
  let pa = Player "P" 1 False (S.fromList [TOrb ('x',1),Key])
  print $ pa
  putStrLn "parses correctly:"
  print $ pa == (read . show) pa
  