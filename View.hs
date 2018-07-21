{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module View where

import Base
import Data.List (intercalate,transpose)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Data.Text (split,pack,unpack)

instance Show EnvObj where
  show (Door n h) = "D"++show n ++ show h
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

padspaces :: [[String]] -> [[String]]
padspaces xss =
  let n = max 1 . maximum . map length
      f txs x = replicate (n txs - length x) ' ' ++ x
  in  transpose $ map (\txs -> map (f txs) txs) (transpose xss)

toStrings:: Show a => MultiSet a -> [String]
toStrings = map show . MS.elems

instance {-# Overlapping #-} Show TimePos where
  show (t,pos) = "[t = " ++ show t ++ ", pos = " ++ show pos ++ "]"
  
-- enclosing encloses each string in the list of strings
-- with z and z' and then joins with the results
enclosing :: String -> String -> [String] -> String
enclosing z z' = intercalate " " . map (\x -> z ++ x ++ z')

toStringMultiMap :: (Show a, Show b) => M.Map a (MultiSet b) -> [String]
toStringMultiMap = concatMap (\(x,ms) -> map (\y -> show y ++ " " ++ show x) $ MS.toAscList ms) . M.toAscList

toStringMultiSet3 :: (Show a, Show b) => MultiSet (a,b,a) -> [String]
toStringMultiSet3 = map (\(a,b,a') -> show a ++" "++show b++" "++show a') . MS.toList

instance Show BlockContent where
  show (BC ps os e) = 
    let firstHalf = intercalate " " $ toStrings ps ++ toStrings os
        finalStr = if null (show e) || null firstHalf
                then firstHalf ++ show e
                else firstHalf ++ " " ++ show e
    in  finalStr
;

-- TODO: explicit instances.
deriving instance Show BC_Cons
deriving instance Show BCT_Cons
deriving instance Show EnvT

instance Show BlockContentT where
  show (BCT (env1,envt,env2) ots pts) =
    let firstHalf = enclosing "[" "]" $ toStringMultiSet3 pts ++ toStringMultiMap ots 
        noChangeEnv = env1 == env2 && envt == EnvStays
        padTo n str = replicate (n - length str) ' ' ++ str
        envStr | noChangeEnv = show env1
               | otherwise   = concat ["[",padTo 1 $ show env1,"] -",show envt,"> [",padTo 1 $ show env2,"]"]
        finalStr = if null envStr || null firstHalf
                then firstHalf ++ envStr
                else firstHalf ++ " " ++ envStr
    in  finalStr
;

instance {-# Overlapping #-} Show (Maybe BlockContent) where
  show = maybe "*" show
;
instance {-# Overlapping #-} Show (Maybe BlockContentT) where -- more compact view of a Field-element
  show = maybe "*" show
;

dot :: (c -> d) -> (a -> b -> c) -> a -> b -> d
dot f g = \x y -> f (g x y)

instance Show PlayerAction where
  show  =
    runpa "<-" "->" "^" "^\\" "/^" "stay"
          (\o -> ("pick("++ show o++")")) (\o -> ("put("++ show o++")"))
          (show `dot` ThrowL) (show `dot` ThrowR) (show . NewTOs) show show
          (\ch (ps,os) t ->
            concat ["tp[",show ch,",",show t,"](",
                        intercalate " " $ toStrings ps ++ toStrings os,")"])

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

show2DMap :: Show a => Space a -> Pos -> String
show2DMap mp (sx,sy) =
  intercalate "\n"
    $ map (intercalate ",")
    $ padspaces
    $ map (\y -> map
                  (\x -> show $ mp M.!(x,y))
                  [0..sx])
          ['A'..sy]
;

showPlayerUnknown2DMap :: Space BlockContentT -> Pos -> String
showPlayerUnknown2DMap mp (sx,sy) =
  intercalate "\n"
    $ map (intercalate ",")
    $ padspaces
    $ map (\y -> map
                  (\x -> maybe "?" show $ M.lookup (x,y) mp)
                  [0..sx])
          ['A'..sy]
;

instance Show PlayerWorld where
  show = showOpenObs

instance Show PlayerWorldT where
  show spo@(Specific t p sz bcmap) =
    showCompactPlayer p ++ " ["
    ++(if peyes p then "Opened eyes view" else "Closed eyes guess")
    ++", t = "++show t++"->"++show(t+1)++"]\n"
    ++ indent 2 (show2DMap bcmap sz)
    ++ (if peyes p then "" else "\nwith " ++ showClosedObsT (reduceToClosedT spo))
;
showClosedObsT :: ClosedObsT -> String
showClosedObsT (Specific t p sz bcmap) =
  showCompactPlayer p ++ " [Closed eyes view, t = "++show t++"]\n"
  ++ indent 2 (showPlayerUnknown2DMap bcmap sz)

-- show player without inventory
showCompactPlayer :: Player -> String
showCompactPlayer p = takeWhile (/='(') . init . tail $ show p

showOpenObs :: OpenObs -> String
showOpenObs opbs@(Specific t p (sx,sy) bcmap) =
  showCompactPlayer p ++ " ["
  ++ (if peyes p then "Opened eyes view" else "Closed eyes guess")
  ++", t = "++show t++"]\n"
  ++ indent 2 (show2DMap bcmap (sx,sy))
  ++ (if peyes p then "" else "\nwith " ++ showClosedObs (reduceToClosed opbs))
;
showClosedObs :: ClosedObs -> String
showClosedObs (Specific t p _ (pos,bc)) =
  showCompactPlayer p ++ " [Closed eyes view"
  ++" at "++show (t,pos)++"]\n"
  ++ indent 2 (show2DMap (M.singleton (0,'A') bc) (0,'A'))
;

indent :: Int -> String -> String
indent n str = tail $ concat . map f . map (:[]) $ "\n"++str
  where f "\n" = "\n" ++ replicate n ' '
        f s    = s

instance Show Player where -- added . at beginning and end for easier parsing
  show (Player s {-age-} o inv) = "." ++ f (s {- ++ show age -}) ++ invStr ++ "."
    where invStr | MS.null inv = ""
                 | otherwise  = "("++(intercalate "+" . map show . MS.toAscList $ inv)++")"
          f x = if not o then "<"++x++">" else x

instance Show PhyObj where
  show Key = "k"
  show (TOrb c i) = 't':c:show i

instance Read Player where
  readsPrec _ ('.':s') =
    if safeLast s' == Just '.' 
      then
        let s = init s'
            opened = head s /= '<'
            inv 
              | length (filter (=='(') s) > 0 = MS.fromList . map (read . unpack) . split (=='+') . pack . reverse . tail . reverse . tail . dropWhile (/='(') $ s
              | otherwise = MS.empty
            nameAgeStr = filter (\x -> notElem x "<>") . takeWhile (/='(') $ s
            -- ageStr = takeWhile (\x -> elem x "0123456789") . reverse $ nameAgeStr
            name = nameAgeStr -- take (length nameAgeStr - length ageStr) nameAgeStr
        in  [(Player name {-(read ageStr)-} opened inv,"")]
    else []
  readsPrec _ _ = []

instance Read PhyObj where
  readsPrec _ "k" = [(Key,"")]
  readsPrec _ ['t',c,i] = [(TOrb c (read $ i:[]),"")]
  readsPrec _ _str = []
;

fromString :: String -> Space BlockContent
fromString = fromNestedList . map (map unpack . split (==',') . pack) . lines

fromNestedList :: [[String]] -> Space BlockContent
fromNestedList =
  M.fromList . concat . zipWith (\y -> zipWith (f y) [0::Int ..]) ['A'..]
  where f y x s = ((x,y),read s)

instance Read EnvObj where
  readsPrec _ "S" = [(Solid,"")]
  readsPrec _ "_" = [(Platform,"")]
  readsPrec _ "" = [(Blank,"")]
  readsPrec _ " " = [(Blank,"")]
  readsPrec _ ['D',n,h] = [(Door (read (n:"")) (read (h:"")),"")]
  readsPrec _ "sw0" = [(Switch False,"")]
  readsPrec _ "sw1" = [(Switch True,"")]
{-readsPrec _ ('m':dir:tm2:tm1:tc2:tc1:rest) =
    [(MovingBlock (read (dir:"")) (read (tm2:tm1:"")) (read (tc2:tc1:"")) (read rest),"")] -}
  readsPrec _ _str = [] -- error $ "readEnv: Could not parse " ++ xs


instance Read BlockContent where
  readsPrec _ "" = [(BC MS.empty MS.empty Blank,"")]
  readsPrec n str =
    let ws = words str
        e = case safeLast ws of
                Nothing -> Blank
                Just rws -> case readsPrec n rws of
                          [] -> Blank
                          ((env,_):_) -> env
        ps = MS.fromList . map fst . concatMap (readsPrec n) $ ws
        os = MS.fromList . map fst . concatMap (readsPrec n) $ ws
    in  [(BC ps os e,"")]
;

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

showSet :: Show a => S.Set a -> String
showSet = intercalate "\n" . map show . S.toList

instance Show ConsHistory where
  show (CH m sz) = 
    case (
      do minT <- maybeToEither "Empty history" . fmap (fst.fst) $ M.minViewWithKey m
         maxT <- maybeToEither "Empty history" . fmap (fst.fst) $ M.maxViewWithKey m
         return $ "History from t="++show minT++" to t=" ++ show maxT ++ " and spacetime-size = "++show sz++":"
                ++ M.foldlWithKey' f "" m ++ "\n\n ====== end of history ========")
    of Left e -> "History invalid due to: " ++ e
       Right x -> x
    where f str t sp =
            str ++ "\n\n  ======= t = " ++ show t ++ " ======= \n"
                ++     "    Static:\n" ++ indent 6 (show2DMap (fst <$> sp) (snd sz))
            ++ "\n    And then:\n"
            ++ indent 6 (show2DMap (snd <$> sp) (snd sz))

instance Show GameState where
  show (GS s ch) =
    case (
      do minT <- maybeToEither "No player observations" . fmap (fst.fst) $ M.minViewWithKey s
         maxT <- maybeToEither "No player observations" . fmap (fst.fst) $  M.maxViewWithKey s
         return $ "GameState. Player Observations from t="++show minT++" to t=" ++ show maxT ++ ":"
                ++ M.foldlWithKey' f "" s ++ "\n\n ====== end of observations ========"
                 ++ "\nwith " ++ show ch)
    of Left e -> "GameState invalid due to: " ++ e
       Right x -> x
    where f str t (pws,pwts) =
            str ++ "\n\n  ======= t = " ++ show t ++ " ======= \n\
            \    Static observations:\n" ++ indent 6 (showSet pws)
            ++ "\n    Then transition observations:\n"
            ++ indent 6 (showSet pwts)