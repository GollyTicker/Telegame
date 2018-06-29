{-# LANGUAGE FlexibleInstances #-}

module View where

import Base
import Data.List (intercalate,transpose)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (split,pack,unpack)

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
  show Blank = ""
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
  (intercalate "\n  "
    $ map (intercalate ",")
    $ padspaces
    $ map (\y -> map
                  (\x -> g (maybe "" id (M.lookup (x,y) info)) (show (mp M.!(x,y))))
                  [0..sx])
          ['A'..sy])
   where  g x y | x == [] || y == [] = x++ y
                | otherwise          = x ++" @ "++ y
          padspaces :: [[String]] -> [[String]]
          padspaces xss =
            let n = max 1 . maximum . map length
                f txs x = replicate (n txs - length x) ' ' ++ x
            in  transpose $ map (\txs -> map (f txs) txs) (transpose xss)

-- reading maps
fromNestedList :: [[String]] -> M.Map Pos EnvObj
fromNestedList =
  M.fromList . concat . zipWith (\y -> zipWith (f y) [0::Int ..]) ['A'..]
  where f y x s = ((x,y),readEnv s)

instance Show (Specific OpenObs) where
  show (Specific t p p_t (OpenObs (Map (sx,sy) mp) os ps)) =
    "Open view of player "++p++show p_t
    ++" at time "++show t++", mapsize "++show (sx,sy)++"\n"
    ++ show2DMapWith infos sx sy mp
    where 
        mergeS x y = x++" "++y
        showSnd (a,b) = (a,show b)
        infos = M.unionWith mergeS
                    (M.fromListWith mergeS . map showSnd . S.toAscList $ os)
                    (M.fromListWith mergeS . map showSnd . S.toAscList $ ps)
          -- maybe todo: mark current player

instance Show (Specific ClosedObs) where
  show (Specific t p p_t (ClosedObs pos env os ps)) =
    "Closed view of player "++p++show p_t
    ++" at time "++show t++" at position "++show pos++"\n"
    ++ show2DMapWith infos 0 'A' (M.singleton (0,'A') env)
    where 
        mergeS x y = x++" "++y
        infos = M.unionWith mergeS
                    (M.fromListWith mergeS . map (\x -> ((0,'A'),show x)) . S.toAscList $ os)
                    (M.fromListWith mergeS . map (\x -> ((0,'A'),show x)) . S.toAscList $ ps)
;


indent n str = concat . map f . map (:[]) $ "\n"++str
  where f "\n" = "\n" ++ replicate n ' '
        f s    = s

instance Show PlayerState where
  show (PSO obs scr) = show obs ++ "\nWith screen:" ++ indent 2 (show scr)
  show (PSC obs scr) = show obs ++ "\nWith screen:" ++ indent 2 (show scr)

instance Show Player where
  show (Player s age o inv) = f (s ++ show age) ++ invStr
    where invStr | S.null inv = ""
                 | otherwise  = "("++(intercalate " " . map show . S.toList $ inv)++")"
          f x = if not o then "<"++x++">" else x

instance Show PhyObj where
  show Key = "k"
  show (TOrb (c,i)) = 't':c:show i

instance Read Player where
  readsPrec n s =
    let opened = head s /= '<'
        inv 
          | length (filter (=='(') s) > 0 = S.fromList . map (read . unpack) . split (==' ') . pack . reverse . tail . reverse . tail . dropWhile (/='(') $ s
          | otherwise = S.empty
        nameAgeStr = filter (\x -> notElem x "<>") . takeWhile (/='(') $ s
        ageStr = takeWhile (\x -> elem x "0123456789") . reverse $ nameAgeStr
        name = take (length nameAgeStr - length ageStr) nameAgeStr
    in  [(Player name (read ageStr) opened inv,"")]

instance Read PhyObj where
  readsPrec n "k" = [(Key,"")]
  readsPrec n ['t',c,i] = [(TOrb (c,read $ i:[]),"")]
  readsPrec n x = []