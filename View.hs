{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, StandaloneDeriving #-}

module View where

import Base
import GameState
import Data.List (intercalate,transpose)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (split,pack,unpack)
import Data.Maybe (fromJust)

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

toStrings:: Show a => S.Set a -> [String]
toStrings = map show . S.elems

toStringTpl :: (Show a, Show b) => S.Set (a,b) -> [String]
toStringTpl = map (\(a,b) -> show a ++ " " ++ show b) . S.elems

instance Show BlockContent where
  show (BC ps os e) = 
    let firstHalf = intercalate " " $ toStrings ps ++ toStrings os
        finalStr = if null (show e) || null firstHalf
                then firstHalf ++ show e
                else firstHalf ++ " " ++ show e
    in  finalStr
;


instance Show BlockContentT where
  show (BCT evch@(env1,env2,mdir) pts ots) =
    let firstHalf = intercalate "|" $ toStringTpl pts ++ toStringTpl ots
        noChangeEnv = env1 == env2 && mdir == Nothing
        padTo n str = replicate (n - length str) ' ' ++ str
        envStr | noChangeEnv = show env1
               | otherwise   = concat ["[",padTo 1 $ show env1,"] -",maybe "" show mdir,"> [",padTo 1 $ show env2,"]"]
        finalStr = if noChangeEnv || null firstHalf
                then firstHalf ++ envStr
                else firstHalf ++ " " ++ envStr
    in  finalStr
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

instance Show PlayerActionT where
  show = runpat (("Init "++). show) -- (("Inter "++). show)
                (\x y -> concat [show (inv x)," then ",show y])
                (("Cmpl "++). show) "CmplFall "
;
instance Show PhyCOT where
  show NoMotionT = "stay"
  show (MotionT inc out) = show (inv inc) ++ " then " ++ show out
  show (TParrive c) = "tpv("++show c++")"
  show (TPexit c) = "tp^("++show c++")"
  show TPsend = "tpx^"
  show TPget = "tpxv"
;
instance Show Dir where
  show = rundir "<" "^" ">" "v"
;

instance Read Dir where
  readsPrec n str = case str of
                      "<" -> [(L,"")]
                      ">" -> [(R,"")]
                      "^" -> [(U,"")]
                      "v" -> [(D,"")]
;

deriving instance Show EARep
deriving instance Show EAOnce

show2DMap :: Show a => Space a -> Pos -> String
show2DMap mp (sx,sy) = "  " ++
  (intercalate "\n  "
    $ map (intercalate ",")
    $ padspaces
    $ map (\y -> map
                  (\x -> show $ mp M.!(x,y))
                  [0..sx])
          ['A'..sy])
;




instance Show PlayerWorld where
  show = showOpenObs

instance Show PlayerWorldT where
  show opbs@(Specific t p (sx,sy) bcmap) =
    "View of transition"
    ++ " of player "++ showCompactPlayer p
    ++" at time "++show t++", mapsize "++show (sx,sy)++"\n"
    ++ show2DMap bcmap (sx,sy)
;

-- show player without inventory
showCompactPlayer p = init . tail . takeWhile (/='(') $ show p

showOpenObs opbs@(Specific t p (sx,sy) bcmap) =
  (if eyesOp p then "Opened eyes view" else "Closed eyes guess")
  ++ " of player "++ showCompactPlayer p
  ++" at time "++show t++", mapsize "++show (sx,sy)++"\n"
  ++ show2DMap bcmap (sx,sy)
  ++ (if eyesOp p then "" else "\nwith " ++ showClosedObs (reduceToClosed opbs))
        -- maybe todo: mark current player

showClosedObs (Specific t p _ (pos,bc)) =
  "Closed eyes view of player "++showCompactPlayer p
  ++" at time "++show t++" at position "++show pos++"\n"
  ++ show2DMap (M.singleton (0,'A') bc) (0,'A')
;

indent :: Int -> String -> String
indent n str = concat . map f . map (:[]) $ "\n"++str
  where f "\n" = "\n" ++ replicate n ' '
        f s    = s

instance Show Player where -- added . at beginning and end for easier parsing
  show (Player s age o inv) = "." ++ f (s ++ show age) ++ invStr ++ "."
    where invStr | S.null inv = ""
                 | otherwise  = "("++(intercalate "+" . map show . S.toList $ inv)++")"
          f x = if not o then "<"++x++">" else x

instance Show PhyObj where
  show Key = "k"
  show (TOrb c i) = 't':c:show i

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
  readsPrec n ['t',c,i] = [(TOrb c (read $ i:[]),"")]
  readsPrec n x = []-- reading maps
;

fromString :: String -> Space BlockContent
fromString = fromNestedList . map (map unpack . split (==',') . pack) . lines

fromNestedList :: [[String]] -> Space BlockContent
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

showSet :: Show a => S.Set a -> String
showSet = intercalate "\n" . map show . S.toList

instance Show GameState where -- TODO: show spacetime. cons history
  show (GS s ch) = "GameState from t="++show minT++" to t=" ++ show maxT ++ ":"
    ++ M.foldlWithKey' f "" s ++ "\n ====== end of history ========"
    where f str t (pws,pwts) =
            str ++ "\n  ======= t = " ++ show t ++ " ======= \n\
            \    Player Worlds:" ++ indent 6 (showSet pws)
            ++ "\n    PlayerWorlds during transition to next time-step:\n"
            ++ indent 6 (showSet pwts)
          minT = maybe (error "GameState map should be non-empty") (fst . fst) $ M.minViewWithKey s
          maxT = maybe (error "GameState map should be non-empty") (fst . fst) $ M.maxViewWithKey s