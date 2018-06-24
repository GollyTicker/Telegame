

module View where

import Base
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S

readEnv :: String -> EnvObj
readEnv "S" = Solid
readEnv "_" = Platform
readEnv "" = Blank
readEnv " " = Blank
readEnv ['D',n,h] = Door (read (n:"")) (read (h:""))
readEnv "sw0" = Switch False
readEnv "sw1" = Switch True
readEnv ('m':dir:tm2:tm1:tc2:tc1:rest) =
  MovingBlock (read (dir:"")) (read (tm2:tm1:"")) (read (tc2:tc1:"")) (readEnv rest)
readEnv xs = error $ "readEnv: Could not parse " ++ xs

instance Show EnvObj where
  show (Door n h) = "D"++show n ++ show h
  show Solid = "S"
  show Blank = " "
  show Platform = "_"
  show (Switch b) = "sw"++show (toEnum . fromEnum $ b :: Int)
  show (MovingBlock dir tm tc behind) =
    "m"++show dir++show0padded tm++show0padded tc++show behind
    where show0padded n
            | n <= 9 = '0':show n
            | otherwise = show n

instance Show Map where
  show (Map (sx,sy) mp) = "Map "++show (sx,sy)++"\n"
    ++ show2DMapWith M.empty sx sy mp

show2DMapWith :: M.Map Pos String -> DX -> DY -> M.Map Pos EnvObj -> String
show2DMapWith info sx sy mp = "  " ++
  (intercalate "\n  " $ map
                         (\y -> intercalate "," $ map
                                                   (\x -> maybe "" id (M.lookup (x,y) info) ++ show (mp M.!(x,y)))
                                                   [0..sx])
                         ['A'..sy])

-- reading maps
fromNestedList :: [[String]] -> M.Map Pos EnvObj
fromNestedList =
  M.fromList . concat . zipWith (\y -> zipWith (f y) [0::Int ..]) ['A'..]
  where f y x s = ((x,y),readEnv s)

instance Show RoomView where
  show (RoomView t (Map (sx,sy) mp) os ps cp) = "RoomView of player "++show cp++" at time "++show t++", mapsize "++show (sx,sy)++"\n"
    ++ show2DMapWith infos sx sy mp
    where 
        mergeS x y = x++" "++y
        showSnd (a,b) = (a,show b)
        infos = M.unionWith mergeS
                    (M.fromListWith mergeS . map showSnd . S.toAscList $ os)
                    (M.fromListWith mergeS . map showSnd . S.toAscList $ ps)
          -- todo: mark current player

instance Show Player where
  show (Player s age inv) = s ++ show age ++ invStr
    where invStr | S.null inv = ""
                 | otherwise  = intercalate " " . map show . S.toList $ inv

instance Show PhyObj where
  show Key = "k"
  show (TOrb (c,i)) = 't':c:show i