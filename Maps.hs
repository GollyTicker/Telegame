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
import Control.Monad

mkP :: {-Int ->-} Bool -> Player
mkP {-i-} eo = Player "P" {-i-} eo MS.empty

one (act,p) = M.singleton p (MS.singleton act)

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 h = fmap (fmap h)

noAction :: BlockContent -> BlockContentT
noAction (BC ps os env) =
  BCT {
     bctenv = (env,EnvStays,env)
    ,bctos = M.fromListWith MS.union $ map (,MS.singleton NoMotionT) $ MS.toList os
    ,bctps = M.fromListWith MS.union $ map (,MS.singleton (Completed NoAction)) $ MS.toList ps
  }
;

map1_P0 = Specific 0 (mkP True) (7,'D') $ fromString
    ".P., ,_, , ,tx1 tx0, D00, \n\
    \  S, , , , ,      S,   S,S\n\
    \  S,S, , , ,      S,   S,S\n\
    \  S,S,S,S,S,      S,   S,S"
;

map1_P1 = Specific 1 (mkP False) (7,'D') $ fromString
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
  . fmap2 noAction $ map2_P0_t0

map2_P0_t1 = Specific 1 (mkP True) (7,'D') $ fromString
    " ,   ,tx1 _, , ,tx0, D00, \n\
    \S,.P.,     , , ,  S,   S,S\n\
    \S,  S,     , , ,  S,   S,S\n\
    \S,  S,    S,S,S,  S,   S,S"
;

map2_P0_t1T = 
  fmap (M.insert (1,'B')   $ BCT (Blank,EnvStays,Blank) M.empty  (one (Initiated  JumpUR,mkP True)))
  . fmap (M.insert (1,'A') $ BCT (Blank,EnvStays,Blank) M.empty  (one (Motion D R       ,mkP True)))
  . fmap (M.insert (2,'A') $ BCT (Platform,EnvStays,Platform) (one (NoMotionT,TOrb 'x' 1)) (one (Completed  JumpUR,mkP True)))
  . fmap2 noAction $ map2_P0_t1

map2_P0_t2 = Specific 2 (mkP True) (7,'D') $ fromString
    " , ,.P. tx1 _, , ,tx0, D00, \n\
    \S, ,     , , ,  S,   S,S\n\
    \S,S,     , , ,  S,   S,S\n\
    \S,S,    S,S,S,  S,   S,S"
;

mytp = Teleport {tpch = 'x', tpobjs = (MS.singleton (mkP True),MS.empty), tpdesttime = 3} 
map2_P0_t2T = 
  fmap (M.insert (2,'A')   $ BCT (Platform,EnvStays,Platform) (one (TPsend,TOrb 'x' 1))  (one (Initiated  mytp,mkP True)))
  . fmap (M.insert (5,'A') $ BCT (Blank,EnvStays,Blank)       (one (TPget,TOrb 'x' 0)) (one (Completed  mytp,mkP True)))
  . fmap2 noAction $ map2_P0_t2

map2_P0_t3 = Specific 3 (mkP True) (7,'D') $ fromString
    " , ,_, , ,.P., D00, \n\
    \S, , , , ,  S,   S,S\n\
    \S,S, , , ,  S,   S,S\n\
    \S,S,S,S,S,  S,   S,S"
;
map2_P0_t3T =
  fmap (M.insert (6,'A')   $ BCT (Blank,EnvStays,Blank) M.empty (one (Initiated MoveR,mkP True)))
  . fmap (M.insert (7,'A') $ BCT (Door 0 0,EnvStays,Door 0 0) M.empty (one (Completed MoveR,mkP True)))
  . fmap2 noAction $ map2_P0_t3

map2_P0_t4 = Specific 4 (mkP True) (7,'D') $ fromString
    " , ,_, , , ,.P. D00, \n\
    \S, , , , ,S,      S,S\n\
    \S,S, , , ,S,      S,S\n\
    \S,S,S,S,S,S,      S,S"
;

map2_P0_t4T =
  fmap (M.insert (6,'A') $ BCT (Door 0 0,EnvStays,Door 0 0) M.empty (one (Completed (UseEnvMult [TraverseDoor]),mkP True)))
  . fmap2 noAction $ map2_P0_t4

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
  in  GS obs $ undefined -- computeCHfromObs obs


main = do
  mapM_ (print >=> const (putStrLn "")) $ [map1_P0, map1_P1]
  putStrLn "Initial Gamestate:"
  print map2_GS
;