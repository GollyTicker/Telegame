{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

module View where

import Base
import Data.List (intercalate,transpose)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (split,pack,unpack)

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
;

padspaces :: [[String]] -> [[String]]
padspaces xss =
  let n = max 1 . maximum . map length
      f txs x = replicate (n txs - length x) ' ' ++ x
  in  transpose $ map (\txs -> map (f txs) txs) (transpose xss)
            
instance Show BlockContent where
  show (BC ps os e) = 
    let 
      toStrings:: Show a => S.Set a -> [String]
      toStrings = map show . S.elems
      firstHalf = intercalate " " $ toStrings ps ++ toStrings os
      finalStr = if null (show e) || null firstHalf
                then firstHalf ++ show e
                else firstHalf ++ " " ++ show e
      in finalStr
;

show2DMap :: M.Map Pos BlockContent -> Pos -> String
show2DMap mp (sx,sy) = "  " ++
  (intercalate "\n  "
    $ map (intercalate ",")
    $ padspaces
    $ map (\y -> map
                  (\x -> show $ mp M.!(x,y))
                  [0..sx])
          ['A'..sy])
;

instance Show (Specific OpenObs) where
  show (Specific t p p_t (Sized (sx,sy) bcmap)) =
    "Open view of player "++p++show p_t
    ++" at time "++show t++", mapsize "++show (sx,sy)++"\n"
    ++ show2DMap bcmap (sx,sy)
          -- maybe todo: mark current player

instance Show (Specific ClosedObs) where
  show (Specific t p p_t (pos,bc)) =
    "Closed view of player "++p++show p_t
    ++" at time "++show t++" at position "++show pos++"\n"
    ++ show2DMap (M.singleton (0,'A') bc) (0,'A')
;


indent n str = concat . map f . map (:[]) $ "\n"++str
  where f "\n" = "\n" ++ replicate n ' '
        f s    = s

{-
todo: write PlayerWorld show instace. use open-predictions -> closed-view reduction
instance Show PlayerState where
  show (PSO obs scr) = show obs ++ "\nWith screen:" ++ indent 2 (show scr)
  show (PSC obs scr) = show obs ++ "\nWith screen:" ++ indent 2 (show scr)
-}

instance Show Player where -- added . at beginning and end for easier parsing
  show (Player s age o inv) = "." ++ f (s ++ show age) ++ invStr ++ "."
    where invStr | S.null inv = ""
                 | otherwise  = "("++(intercalate "+" . map show . S.toList $ inv)++")"
          f x = if not o then "<"++x++">" else x

instance Show PhyObj where
  show Key = "k"
  show (TOrb (c,i)) = 't':c:show i

instance Read Player where
  readsPrec n ('.':s') =
    if safeLast s' == Just '.' 
      then
        let s = init s'
            opened = head s /= '<'
            inv 
              | length (filter (=='(') s) > 0 = S.fromList . map (read . unpack) . split (=='+') . pack . reverse . tail . reverse . tail . dropWhile (/='(') $ s
              | otherwise = S.empty
            nameAgeStr = filter (\x -> notElem x "<>") . takeWhile (/='(') $ s
            ageStr = takeWhile (\x -> elem x "0123456789") . reverse $ nameAgeStr
            name = take (length nameAgeStr - length ageStr) nameAgeStr
        in  [(Player name (read ageStr) opened inv,"")]
    else []
  readsPrec n _ = []

instance Read PhyObj where
  readsPrec n "k" = [(Key,"")]
  readsPrec n ['t',c,i] = [(TOrb (c,read $ i:[]),"")]
  readsPrec n x = []-- reading maps
;

fromNestedList :: [[String]] -> M.Map Pos BlockContent
fromNestedList =
  M.fromList . concat . zipWith (\y -> zipWith (f y) [0::Int ..]) ['A'..]
  where f y x s = ((x,y),read s)

instance Read EnvObj where
  readsPrec n "S" = [(Solid,"")]
  readsPrec n "_" = [(Platform,"")]
  readsPrec n "" = [(Blank,"")]
  readsPrec n " " = [(Blank,"")]
  readsPrec _ ['D',n,h] = [(Door (read (n:"")) (read (h:"")),"")]
  readsPrec n "sw0" = [(Switch False,"")]
  readsPrec n "sw1" = [(Switch True,"")]
  readsPrec n ('m':dir:tm2:tm1:tc2:tc1:rest) =
    [(MovingBlock (read (dir:"")) (read (tm2:tm1:"")) (read (tc2:tc1:"")) (read rest),"")]
  readsPrec _ xs = [] -- error $ "readEnv: Could not parse " ++ xs


instance Read BlockContent where
  readsPrec n "" = [(BC S.empty S.empty Blank,"")]
  readsPrec n str =
    let ws = words str
        e = case safeLast ws of
                Nothing -> Blank
                Just rws -> case readsPrec n rws of
                          [] -> Blank
                          ((env,_):_) -> env
        ps = S.fromList . map fst . concatMap (readsPrec n) $ ws
        os = S.fromList . map fst . concatMap (readsPrec n) $ ws
    in  [(BC ps os e,"")]
;

safeLast [] = Nothing
safeLast xs = Just $ last xs

safeTail (_:xs) = xs
safeTail [] = []