{-# LANGUAGE FlexibleContexts, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

module Maps
  where

import Base
import View
import GameState
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.MultiSet as MS

mkP :: {-Int ->-} Bool -> Player
mkP {-i-} eo = Player "P" {-i-} eo MS.empty

mkPwith :: Bool -> [PhyObj] -> Player
mkPwith eo ls = Player "P" eo (MS.fromList ls)

one (act,p) = MS.singleton (p,act,p)
oneO (mot,o) = M.singleton o (MS.singleton mot)

noAction :: (Functor f, Functor g) => f (g BlockSt) -> f (g BlockTr)
noAction = fmap (fmap f)
  where f (BC ps os env) = BCT {
       bctenv = (env,EnvStays,env)
      ,bctos = M.fromListWith MS.union $ map (,MS.singleton NoMotionT) $ MS.toList os
      ,bctps = MS.map (\p -> (p,Completed NoAction,p)) ps
    }
;

map1_P0 = Specific 0 (mkP True) (7,'D') $ fromString
    ".P., ,_, , ,tx1 tx0, D00, \n\
    \  S, , , , ,      S,   S,S\n\
    \  S,S, , , ,      S,   S,S\n\
    \  S,S,S,S,S,      S,   S,S"
;

map1_P1 = Specific 1 (mkPwith False [Key]) (7,'D') $ fromString
    " , ,_, , , , .<P>(k). tx0 tx1 D00, \n\
    \S, , , , , S,   S,S\n\
    \S,S, , , , S,   S,S\n\
    \S,S,S,S,S, S,   S,S"

map2_P0_t0 = Specific 0 (mkP True) (7,'D') $ fromString
    ".P., ,tx1 _, , ,tx0, D00, \n\
    \  S, ,     , , ,  S,   S,S\n\
    \  S,S,     , , ,  S,   S,S\n\
    \  S,S,    S,S,S,  S,   S,S"

map2_P0_t0T =
  fmap (M.insert (0,'A')   $ BCT (Blank,EnvStays,Blank) M.empty (one . (Initiated    MoveR,) $ mkP True))
  . fmap (M.insert (1,'A') $ BCT (Blank,EnvStays,Blank) M.empty (one . (Motion L D        ,) $ mkP True))
  . fmap (M.insert (1,'B') $ BCT (Blank,EnvStays,Blank) M.empty (one . (CompletedFalling  ,) $ mkP True))
  $ noAction map2_P0_t0

map2_P0_t1 = Specific 1 (mkP True) (7,'D') $ fromString
    " ,   ,tx1 _, , ,tx0, D00, \n\
    \S,.P.,     , , ,  S,   S,S\n\
    \S,  S,     , , ,  S,   S,S\n\
    \S,  S,    S,S,S,  S,   S,S"
;

map2_P0_t1T = 
  fmap (M.insert (1,'B')   $ BCT (Blank,EnvStays,Blank) M.empty  (one (Initiated  JumpUR,mkP True)))
  . fmap (M.insert (1,'A') $ BCT (Blank,EnvStays,Blank) M.empty  (one (Motion D R       ,mkP True)))
  . fmap (M.insert (2,'A') $ BCT (Platform,EnvStays,Platform) (oneO (NoMotionT,TOrb 'x' 1)) (one (Completed  JumpUR,mkP True)))
  $ noAction map2_P0_t1

map2_P0_t2 = Specific 2 (mkP True) (7,'D') $ fromString
    " , ,.P. tx1 _, , ,tx0, D00, \n\
    \S, ,     , , ,  S,   S,S\n\
    \S,S,     , , ,  S,   S,S\n\
    \S,S,    S,S,S,  S,   S,S"
;

mytp = Teleport {tpch = 'x', tpobjs = (MS.singleton (mkP True),MS.empty), tpdesttime = 3} 
map2_P0_t2T = 
  fmap (M.insert (2,'A')   $ BCT (Platform,EnvStays,Platform) (oneO (TPsend,TOrb 'x' 1))  (one (Initiated  mytp,mkP True)))
  . fmap (M.insert (5,'A') $ BCT (Blank,EnvStays,Blank)       (oneO (TPget,TOrb 'x' 0)) (one (Completed  mytp,mkP True)))
  $ noAction map2_P0_t2

map2_P0_t3 = Specific 3 (mkP True) (7,'D') $ fromString
    " , ,_, , ,.P., D00, \n\
    \S, , , , ,  S,   S,S\n\
    \S,S, , , ,  S,   S,S\n\
    \S,S,S,S,S,  S,   S,S"
;
map2_P0_t3T =
  fmap (M.insert (6,'A')   $ BCT (Door 0 0,EnvStays,Door 0 0) M.empty (one (Initiated MoveR,mkP True)))
  . fmap (M.insert (7,'A') $ BCT (Blank,EnvStays,Blank) M.empty (one (Completed MoveR,mkP True)))
  $ noAction map2_P0_t3

map2_P0_t4 = Specific 4 (mkP True) (7,'D') $ fromString
    " , ,_, , , ,.P. D00, \n\
    \S, , , , ,S,      S,S\n\
    \S,S, , , ,S,      S,S\n\
    \S,S,S,S,S,S,      S,S"
;

map2_P0_t4T =
  fmap (M.insert (6,'A') $ BCT (Door 0 0,EnvStays,Door 0 0) M.empty (one (Completed (UseEnvMult [TraverseDoor]),mkP True)))
  $ noAction map2_P0_t4

map2_initGS :: GameState
map2_initGS = either (error . ("map2_initGS: "++)) id $ initGS (S.singleton map2_P0_t0)

map2_GS :: GameState
map2_GS =
  let obs = (M.fromList
            [(0,(f map2_P0_t0,f map2_P0_t0T)),
             (1,(f map2_P0_t1,f map2_P0_t1T)),
             (2,(f map2_P0_t2,f map2_P0_t2T)),
             (3,(f map2_P0_t3,f map2_P0_t3T)),
             (4,(f map2_P0_t4,f map2_P0_t4T))])
      f = S.singleton
  in  either (error . ("map2_GS: "++)) id $ GS obs <$> computeCHfromObs obs

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
  mapM_ putStrLn . contradictions $ gsch map2_GS
  
  -- putStrLn "Minimal GameState:" >> print map2_initGS
;
