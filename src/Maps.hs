{-# LANGUAGE FlexibleContexts, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

module Maps
  where

import BaseBlock
import GameState
import View (fromString)
import qualified Data.Map as M
import qualified Data.MultiSet as MS

mkP :: {-Int ->-} Bool -> Player
mkP {-i-} eo = Player "P" {-i-} eo MS.empty

mkPwith :: Bool -> [PhyObj] -> Player
mkPwith eo ls = Player "P" eo (MS.fromList ls)

one (act,p) = MS.singleton (p,act,p)
oneO (mot,o) = M.singleton o (MS.singleton mot)

map1_P0 = Specific 0 (mkP True) (7,'D') $ fromString
    ".P., ,_, , ,ta1 ta0, D00, \n\
    \  S, , , , ,      S,   S,S\n\
    \  S,S, , , ,      S,   S,S\n\
    \  S,S,S,S,S,      S,   S,S"
;

map1_P1 = Specific 1 (mkPwith False [Key]) (7,'D') $ fromString
    " , ,_, , , , .<P>(k). ta0 ta1 D00, \n\
    \S, , , , , S,   S,S\n\
    \S,S, , , , S,   S,S\n\
    \S,S,S,S,S, S,   S,S"

map2_P0_t0 = Specific 0 (mkP True) (7,'D') $ fromString
    ".P., ,ta1 _, , ,ta0, D00, \n\
    \  S, ,     , , ,  S,   S,S\n\
    \  S,S,     , , ,  S,   S,S\n\
    \  S,S,    S,S,S,  S,   S,S"

map2_P0_t0T =
  fmap (M.insert (0,'A')   $ BCT (Blank,EnvStays,Blank) MS.empty (one . (Initiated    MoveR,) $ mkP True))
  . fmap (M.insert (1,'A') $ BCT (Blank,EnvStays,Blank) MS.empty (one . (Motion L D        ,) $ mkP True))
  . fmap (M.insert (1,'B') $ BCT (Blank,EnvStays,Blank) MS.empty (one . (CompletedFalling  ,) $ mkP True))
  $ noAction map2_P0_t0

map2_P0_t1 = Specific 1 (mkP True) (7,'D') $ fromString
    " ,   ,ta1 _, , ,ta0, D00, \n\
    \S,.P.,     , , ,  S,   S,S\n\
    \S,  S,     , , ,  S,   S,S\n\
    \S,  S,    S,S,S,  S,   S,S"
;

map2_P0_t1T =
  fmap (M.insert (1,'B')   $ BCT (Blank,EnvStays,Blank) MS.empty  (one (Initiated  JumpUR,mkP True)))
  . fmap (M.insert (1,'A') $ BCT (Blank,EnvStays,Blank) MS.empty  (one (Motion D R       ,mkP True)))
  . fmap (M.insert (2,'A') $ BCT (Platform,EnvStays,Platform) (one (NoMotionT,TOrb 'a' 1)) (one (Completed  JumpUR,mkP True)))
  $ noAction map2_P0_t1

map2_P0_t2 = Specific 2 (mkP True) (7,'D') $ fromString
    " , ,.P. ta1 _, , ,ta0, D00, \n\
    \S, ,     , , ,  S,   S,S\n\
    \S,S,     , , ,  S,   S,S\n\
    \S,S,    S,S,S,  S,   S,S"
;

mytp = TP True $ Teleport {tpch = 'a', tpobjs = (MS.singleton (mkP True),MS.empty), tpsource = (2,(2,'A')), tpdest = (3,(5,'A'))}
map2_P0_t2T =
  fmap (M.insert (2,'A')   $ BCT (Platform,EnvStays,Platform) (one (TPsend,TOrb 'a' 1))  (one (Initiated  mytp,mkP True)))
  . fmap (M.insert (5,'A') $ BCT (Blank,EnvStays,Blank)       (one (TPget,TOrb 'a' 0)) (one (Completed  mytp,mkP True)))
  $ noAction map2_P0_t2

map2_P0_t3 = Specific 3 (mkP True) (7,'D') $ fromString
    " , ,_, , ,.P., D00, \n\
    \S, , , , ,  S,   S,S\n\
    \S,S, , , ,  S,   S,S\n\
    \S,S,S,S,S,  S,   S,S"
;
map2_P0_t3T =
  fmap (M.insert (5,'A')   $ BCT (Blank,EnvStays,Blank) MS.empty (one (Initiated MoveR,mkP True)))
  . fmap (M.insert (6,'A') $ BCT (Door 0 0,EnvStays,Door 0 0) MS.empty (one (Completed MoveR,mkP True)))
  $ noAction map2_P0_t3

map2_P0_t4 = Specific 4 (mkP True) (7,'D') $ fromString
    " , ,_, , , ,.P. D00, \n\
    \S, , , , ,S,      S,S\n\
    \S,S, , , ,S,      S,S\n\
    \S,S,S,S,S,S,      S,S"
;

map2_P0_t4T =
  fmap (M.insert (6,'A') $ BCT (Door 0 0,EnvStays,Door 0 0) MS.empty (one (Completed (UseEnvMult [TraverseDoor]),mkP True)))
  $ noAction map2_P0_t4

map2_initGS :: GameState
map2_initGS = either (error . ("map2_initGS: "++) . head) id $ initGS (MS.singleton map2_P0_t0)

map2_GS :: GameState
map2_GS =
  let obs = (M.fromList
            [(0,(f map2_P0_t0,f map2_P0_t0T)),
             (1,(f map2_P0_t1,f map2_P0_t1T)),
             (2,(f map2_P0_t2,f map2_P0_t2T)),
             (3,(f map2_P0_t3,f map2_P0_t3T)),
             (4,(f map2_P0_t4,f map2_P0_t4T))])
      f = MS.singleton
  in  either (error . ("map2_GS: "++) . head) id $ mkGSfromObs obs

main :: IO ()
main = do
  putStrLn "Some Open/Closed Observations:"
  print map1_P0 >> putChar '\n'
  print map1_P1 >> putChar '\n'
  print (noAction map1_P0)>> putChar '\n'
  print (noAction map1_P1)>> putChar '\n'
  putChar '\n'

  putStrLn "\n\nInitial GameState:"
  print map2_GS

  putStrLn "\nWith Contradictions:"
  let contras = contradictions $ (gsch map2_GS)
  let formatter = fst . foldl (\(s,i) c -> if c=='{' then (s++'\n':replicate (i+1) ' '++"{",i+1) else (if c=='}' then (s++"}\n"++replicate (i-1) ' ',i-1) else (s++[c],i))) ("",0)
  mapM_ (\x -> putStrLn (formatter x) >> putStrLn "") contras
  putStrLn $ "\n" ++ show (length contras) ++ " contradictions found."

  -- putStrLn "Minimal GameState:" >> print map2_initGS
;
