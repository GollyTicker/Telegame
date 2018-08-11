{-# LANGUAGE FlexibleInstances, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

{- view instances for simples types. needed by Interference.hs for show on error -}

module ViewBase(
     split
    ,toStrings
    ,safeLast
    ,enclosing
    ,dot
  )
  where

import BaseBlock
import Data.List (intercalate)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS

instance Show Player where -- added . at beginning and end for easier parsing
  show (Player s {-age-} o inv) = "." ++ f (s {- ++ show age -}) ++ invStr ++ "."
    where invStr | MS.null inv = ""
                 | otherwise  = "("++(intercalate "+" . map show . MS.toAscList $ inv)++")"
          f x = if not o then "<"++x++">" else x

instance Show PhyObj where
  show Key = "k"
  show (TOrb c i) = 't':c:show i
;

instance Show Env where
  show (Door n' h) = "D"++show n' ++ show h
  show Solid = "S"
  show Blank = ""
  show Platform = "_"
  show (Switch b) = "sw"++show (toEnum . fromEnum $ b :: Int)
{-show (MovingBlock dir tm tc behind) =
    "m"++show dir++show0padded tm++show0padded tc++show behind
    where show0padded n
            | n <= 9 = '0':show n
            | otherwise = show n -}
;

split :: (a -> Bool) -> [a] -> [[a]]
split f s = foldr (\x (a:as) -> if f x then []:(a:as) else (x:a):as) [[]] s

toStrings:: Show a => MultiSet a -> [String]
toStrings = map show . MS.elems

-- enclosing encloses each string in the list of strings
-- with z and z' and then joins with the results
enclosing :: String -> String -> String -> [String] -> String
enclosing z i z' = intercalate i . map (\x -> z ++ x ++ z')

instance {-# Overlapping #-} Show TimePos where
  show (t,pos) = "[t = " ++ show t ++ ", pos = " ++ show pos ++ "]"
;

instance Show (This BlockSt) where
  show _ = "state"
;

instance Show (This BlockTr) where
  show _ = "trans"

instance {-# Overlapping #-} Block b => Show (TimePos,This b) where
  show ((t,pos),ths) = "[t = " ++ show t ++ ", pos = " ++ show pos ++ " in " ++ show ths++"]"
;


dot :: (c -> d) -> (a -> b -> c) -> a -> b -> d
dot f g = \x y -> f (g x y)

showTPosCompact :: TimePos -> String
showTPosCompact (t,(x,y)) = concat [show t,",",show x,",",show y]

instance Show PlayerAction where
  show  =
    runpa "<-" "->" "^" "^\\" "/^" "stay"
          (\o -> ("pick("++ show o++")")) (\o -> ("put("++ show o++")"))
          (show `dot` ThrowL) (show `dot` ThrowR) (show . NewTOs) show show
          (\b tp -> show tp ++ if b then "+self" else "")
          (\tc -> "tpv("++show tc++")") (\tc -> "tp^("++show tc++")")

instance Show Teleport where
  show (Teleport ch (ps,os) ts td) =
    concat ["tp[",show ch,"|",showTPosCompact ts,"->"++showTPosCompact td++"](",
      intercalate " " $ toStrings ps ++ toStrings os,")"]

instance Show PlayerT where
  show = runpat (("Init "++). show) -- (("Inter "++). show)
                (\x y -> concat [show (invDir x)," then ",show y])
                (("Cmpl "++). show) "CmplFall "
;
instance Show PhyObjT where
  show NoMotionT = "stay"
  show (MotionT inc out) = show (invDir inc) ++ " then " ++ show out
  show (TParrive c) = "tpv("++show c++")"
  show (TPexit c) = "tp^("++show c++")"
  show TPsend = "tpx^"
  show TPget = "tpxv"
  show (LandFrom dir) = "\\"++show dir++"/"
  show IntoInventory = "(^)"
  show OntoGround    = "(v)"
;
instance Show Dir where
  show = rundir "<" "^" ">" "v"
;

instance Read Dir where
  readsPrec _ str = case str of
                      "<" -> [(L,"")]
                      ">" -> [(R,"")]
                      "^" -> [(U,"")]
                      "v" -> [(D,"")]
                      _   -> []
;

deriving instance Show EARep
deriving instance Show EAOnce

instance Read Player where
  readsPrec _ ('.':s') =
    if safeLast s' == Just '.' 
      then
        let s = init s'
            opened = head s /= '<'
            inv 
              | length (filter (=='(') s) > 0 = MS.fromList . map read . split (=='+') . reverse . tail . reverse . tail . dropWhile (/='(') $ s
              | otherwise = MS.empty
            nameAgeStr = filter (\x -> notElem x "<>") . takeWhile (/='(') $ s
            -- ageStr = takeWhile (\x -> elem x "0123456789") . reverse $ nameAgeStr
            name = nameAgeStr -- take (length nameAgeStr - length ageStr) nameAgeStr
        in  [(Player name {-(read ageStr)-} opened inv,"")]
    else []
  readsPrec _ _ = []

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

instance Read PhyObj where
  readsPrec _ "k" = [(Key,"")]
  readsPrec _ ['t',c,i] = [(TOrb c (read $ i:[]),"")]
  readsPrec _ _str = []
;

instance Read Env where
  readsPrec _ "S" = [(Solid,"")]
  readsPrec _ "_" = [(Platform,"")]
  readsPrec _ "" = [(Blank,"")]
  readsPrec _ " " = [(Blank,"")]
  readsPrec _ ['D',n',h] = [(Door (read (n':"")) (read (h:"")),"")]
  readsPrec _ "sw0" = [(Switch False,"")]
  readsPrec _ "sw1" = [(Switch True,"")]
{-readsPrec _ ('m':dir:tm2:tm1:tc2:tc1:rest) =
    [(MovingBlock (read (dir:"")) (read (tm2:tm1:"")) (read (tc2:tc1:"")) (read rest),"")] -}
  readsPrec _ _str = [] -- error $ "readEnv: Could not parse " ++ xs
;
