{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module View (
    fromString
  )
  where

import Interference
import ViewBase
import Data.Typeable (cast)
import Data.Maybe (fromJust)
import Data.List (intercalate,transpose)
import qualified Data.Map as M
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Data.Foldable

padspaces :: [[String]] -> [[String]]
padspaces xss =
  let n = max 1 . maximum . map length
      f txs x = replicate (n txs - length x) ' ' ++ x
  in  transpose $ map (\txs -> map (f txs) txs) (transpose xss)

-- enclosing encloses each string in the list of strings
-- with z and z' and then joins with the results
enclosing :: String -> String -> [String] -> String
enclosing z z' = intercalate " " . map (\x -> z ++ x ++ z')

toStringMultiMap :: (Show a, Show b) => M.Map a (MultiSet b) -> [String]
toStringMultiMap = concatMap (\(x,ms) -> map (\y -> show y ++ " " ++ show x) $ MS.toAscList ms) . M.toAscList

toStringMultiSet3 :: (Show a, Show b) => MultiSet (a,b,a) -> [String]
toStringMultiSet3 = map (\(a,b,a') -> show a ++" "++show b++" "++show a') . MS.toList

instance Show BlockSt where
  show (BC ps os e) = 
    let firstHalf = intercalate " " $ toStrings ps ++ toStrings os
        finalStr = if null (show e) || null firstHalf
                then firstHalf ++ show e
                else firstHalf ++ " " ++ show e
    in  finalStr
;

-- TODO: explicit instances.
deriving instance Show (Cons BlockSt)
deriving instance Show (Cons BlockTr)
deriving instance Show EnvT
deriving instance Show PlayerInput

instance Show ConsResB where
  show (CR (consres::ConsRes b)) =
    let str = if (this :: This b) `is_a` St
          then show (fromJust $ cast consres :: ConsRes BlockSt)
          else show (fromJust $ cast consres :: ConsRes BlockTr)
    in  concat ["CR (",str,")"]
  
instance Show BlockTr where
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

instance {-# Overlapping #-} Show (Maybe BlockSt) where
  show = maybe "*" show
;
instance {-# Overlapping #-} Show (Maybe BlockTr) where -- more compact view of a Field-element
  show = maybe "*" show
;


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

showPlayerUnknown2DMap :: Space BlockTr -> Pos -> String
showPlayerUnknown2DMap mp (sx,sy) =
  intercalate "\n"
    $ map (intercalate ",")
    $ padspaces
    $ map (\y -> map
                  (\x -> maybe "?" show $ M.lookup (x,y) mp)
                  [0..sx])
          ['A'..sy]
;

instance Show (Specific (Space BlockSt)) where
  show = showOpenObs

instance Show (Specific (Space BlockTr)) where
  show spo@(Specific t p sz bcmap) =
    showCompactPlayer p ++ " ["
    ++(if peyes p then "Opened eyes view" else "Closed eyes guess")
    ++", t = "++show t++"->"++show(t+1)++"]\n"
    ++ indent 2 (show2DMap bcmap sz)
    ++ (if peyes p then "" else "\nwith " ++ showClosedObsT (reduceToClosed (Proxy::Proxy BlockTr) spo))
;
showClosedObsT :: ClosedObs BlockTr -> String
showClosedObsT (Specific t p sz bcmap) =
  showCompactPlayer p ++ " [Closed eyes view, t = "++show t++"]\n"
  ++ indent 2 (showPlayerUnknown2DMap bcmap sz)

-- show player without inventory
showCompactPlayer :: Player -> String
showCompactPlayer p = takeWhile (/='(') . init . tail $ show p

showOpenObs :: OpenObs BlockSt -> String
showOpenObs opbs@(Specific t p (sx,sy) bcmap) =
  showCompactPlayer p ++ " ["
  ++ (if peyes p then "Opened eyes view" else "Closed eyes guess")
  ++", t = "++show t++"]\n"
  ++ indent 2 (show2DMap bcmap (sx,sy))
  ++ (if peyes p then "" else "\nwith " ++ showClosedObs (reduceToClosed (Proxy::Proxy BlockSt) opbs))
;
showClosedObs :: ClosedObs BlockSt -> String
showClosedObs (Specific t p _ (pos,bc)) =
  showCompactPlayer p ++ " [Closed eyes view"
  ++" at "++show (t,pos)++"]\n"
  ++ indent 2 (show2DMap (M.singleton (0,'A') bc) (0,'A'))
;

indent :: Int -> String -> String
indent n str = tail $ concat . map f . map (:[]) $ "\n"++str
  where f "\n" = "\n" ++ replicate n ' '
        f s    = s

fromString :: String -> Space BlockSt
fromString = fromNestedList . map (split (==',')) . lines

fromNestedList :: [[String]] -> Space BlockSt
fromNestedList =
  M.fromList . concat . zipWith (\y -> zipWith (f y) [0::Int ..]) ['A'..]
  where f y x s = ((x,y),read s)

instance Read BlockSt where
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

showFld :: (Foldable t,Show a) => t a -> String
showFld = intercalate "\n" . map show . toList

instance Show ConsHistory where
  show (CH m sz g) = 
    case (
      do minT <- maybeToEither "Empty history" . fmap (fst.fst) $ M.minViewWithKey m
         maxT <- maybeToEither "Empty history" . fmap (fst.fst) $ M.maxViewWithKey m
         return $ "History from t="++show minT++" to t=" ++ show maxT ++ " and spacetime-size = "++show sz++":"
                ++ M.foldlWithKey' f "" m
                ++"\n\nwith global information:"
                ++ (if M.null g then " none" else M.foldlWithKey' (\s _ e-> s++"\n"++show e) "" g)
                ++"\n\n====== end of history ========")
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
            \    Static observations:\n" ++ indent 6 (showFld pws)
            ++ "\n    Then transition observations:\n"
            ++ indent 6 (showFld pwts)
