{-# LANGUAGE FlexibleContexts, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

module Maps
  where

import Base
import View
import GameState
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad

mkP :: Int -> Bool -> Player
mkP i eo = Player "P" i eo S.empty

one = S.singleton

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 h = fmap (fmap h)

noAction :: BlockContent -> BlockContentT
noAction (BC ps os env) =
  BCT {
     bctenvs = (env,env,Nothing)
    ,bctos = S.map (NoMotionT,) os
    ,bctps = S.map (Completed NoAction,) ps
  }
;

map1_P0 = Specific 0 (mkP 0 True) (7,'D') $ fromString
    ".P0., ,_, , ,tx1 tx0, D00, \n\
    \   S, , , , ,      S,   S,S\n\
    \   S,S, , , ,      S,   S,S\n\
    \   S,S,S,S,S,      S,   S,S"
;

map1_P1 = Specific 1 (mkP 1 False) (7,'D') $ fromString
    " , ,_, , , , .<P1>(k). tx0 tx1 D00, \n\
    \S, , , , , S,   S,S\n\
    \S,S, , , , S,   S,S\n\
    \S,S,S,S,S, S,   S,S"

map2_P0_t0 = Specific 0 (mkP 0 True) (7,'D') $ fromString
    ".P0., ,tx1 _, , ,tx0, D00, \n\
    \   S, ,     , , ,  S,   S,S\n\
    \   S,S,     , , ,  S,   S,S\n\
    \   S,S,    S,S,S,  S,   S,S"

map2_P0_t0T =
  fmap (M.insert (0,'A')   $ BCT (Blank,Blank,Nothing) S.empty (one . (Initiated    MoveR,) $ mkP 0 True))
  . fmap (M.insert (1,'A') $ BCT (Blank,Blank,Nothing) S.empty (one . (Motion L D        ,) $ mkP 0 True))
  . fmap (M.insert (1,'B') $ BCT (Blank,Blank,Nothing) S.empty (one . (CompletedFalling  ,) $ mkP 0 True))
  . fmap2 noAction $ map2_P0_t0

map2_P0_t1 = Specific 1 (mkP 1 True) (7,'D') $ fromString
    " ,    ,tx1 _, , ,tx0, D00, \n\
    \S,.P1.,     , , ,  S,   S,S\n\
    \S,   S,     , , ,  S,   S,S\n\
    \S,   S,    S,S,S,  S,   S,S"
;

map2_P0_t1T = 
  fmap (M.insert (1,'B')   $ BCT (Blank,Blank,Nothing) S.empty  (one (Initiated  JumpUR,mkP 1 True)))
  . fmap (M.insert (1,'A') $ BCT (Blank,Blank,Nothing) S.empty  (one (Motion D R       ,mkP 1 True)))
  . fmap (M.insert (2,'A') $ BCT (Platform,Platform,Nothing) (one (NoMotionT,TOrb 'x' 1)) (one (Completed  JumpUR,mkP 1 True)))
  . fmap2 noAction $ map2_P0_t1

map2_P0_t2 = Specific 2 (mkP 2 True) (7,'D') $ fromString
    " , ,.P2. tx1 _, , ,tx0, D00, \n\
    \S, ,     , , ,  S,   S,S\n\
    \S,S,     , , ,  S,   S,S\n\
    \S,S,    S,S,S,  S,   S,S"
;

mytp = Teleport {tpch = 'x', tpobjs = (one (mkP 2 True),S.empty), tpdesttime = 3} 
map2_P0_t2T = 
  fmap (M.insert (2,'A')   $ BCT (Platform,Platform,Nothing) (one (TPsend,TOrb 'x' 1))  (one (Initiated  mytp,mkP 2 True)))
  . fmap (M.insert (5,'A') $ BCT (Blank,Blank,Nothing)       (one (TPget,TOrb 'x' 0)) (one (Completed  mytp,mkP 2 True)))
  . fmap2 noAction $ map2_P0_t2

map2_P0_t3 = Specific 3 (mkP 3 True) (7,'D') $ fromString
    " , ,_, , ,.P3., D00, \n\
    \S, , , , ,   S,   S,S\n\
    \S,S, , , ,   S,   S,S\n\
    \S,S,S,S,S,   S,   S,S"
;
map2_P0_t3T =
  fmap (M.insert (6,'A')   $ BCT (Blank,Blank,Nothing) S.empty (one (Initiated MoveR,mkP 3 True)))
  . fmap (M.insert (7,'A') $ BCT (Door 0 0,Door 0 0,Nothing) S.empty (one (Completed MoveR,mkP 3 True)))
  . fmap2 noAction $ map2_P0_t3

map2_P0_t4 = Specific 4 (mkP 4 True) (7,'D') $ fromString
    " , ,_, , , ,.P4. D00, \n\
    \S, , , , ,S,   S,S\n\
    \S,S, , , ,S,   S,S\n\
    \S,S,S,S,S,S,   S,S"
;
map2_P0_t4T =
  fmap (M.insert (6,'A') $ BCT (Door 0 0,Door 0 0,Nothing) S.empty (one (Completed (UseEnvMult [TraverseDoor]),mkP 4 True)))
  . fmap2 noAction $ map2_P0_t4

map2_initGS :: GameState
map2_initGS = either (error . ("map2_initGS: "++)) id $ initGS (S.singleton map2_P0_t0)

map2_GS :: GameState
map2_GS =
  let obs = (M.fromList
            [(0,(one map2_P0_t0,one map2_P0_t0T)),
             (1,(one map2_P0_t1,one map2_P0_t1T)),
             (2,(one map2_P0_t2,one map2_P0_t2T)),
             (3,(one map2_P0_t3,one map2_P0_t3T)),
             (4,(one map2_P0_t4,one map2_P0_t4T))])
  in  GS obs $ undefined -- computeCHfromObs obs


main = do
  mapM_ (print >=> const (putStrLn "")) $ [map1_P0, map1_P1]
  putStrLn "Initial Gamestate:"
  print map2_GS
;