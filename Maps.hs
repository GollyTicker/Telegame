
module Maps
  where

import Base
import View
import qualified Data.Set as S
import qualified Data.Map as M

map1_Just_in_Reach = Map { size =(7,'D'),
  env = fromNestedList
    [
       ["","","_","","","","D00",""]
      ,["S","","","","","S","S","S"]
      ,["S","S","","","","S","S","S"]
      ,["S","S","S","S","S","S","S","S"]
    ]
}

map1_Just_in_Reach_init =
  [RoomView 0 map1_Just_in_Reach
    (S.fromList $ map (\i -> ((5,'A'), TOrb ('x',i))) [0,1])
    (S.singleton ((0,'A'),Player "P" 0 S.empty))
    ("P",0)
  ]

main = print $ map1_Just_in_Reach_init