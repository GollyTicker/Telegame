{-# LANGUAGE RankNTypes, ImpredicativeTypes, ExistentialQuantification, FlexibleContexts #-}

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

map2_P0_t0T = undefined

map2_P0_t1 = Specific 1 (mkP 1 True) (7,'D') $ fromString
    " ,    ,tx1 _, , ,tx0, D00, \n\
    \S,.P1.,     , , ,  S,   S,S\n\
    \S,   S,     , , ,  S,   S,S\n\
    \S,   S,    S,S,S,  S,   S,S"
;

map2_P0_t1T = undefined

map2_P0_t2 = Specific 2 (mkP 2 True) (7,'D') $ fromString
    " , ,.P2. tx1 _, , ,tx0, D00, \n\
    \S, ,     , , ,  S,   S,S\n\
    \S,S,     , , ,  S,   S,S\n\
    \S,S,    S,S,S,  S,   S,S"
;
map2_P0_t2T = undefined

map2_P0_t3 = Specific 3 (mkP 3 True) (7,'D') $ fromString
    " , ,_, , ,.P3., D00, \n\
    \S, , , , ,   S,   S,S\n\
    \S,S, , , ,   S,   S,S\n\
    \S,S,S,S,S,   S,   S,S"
;
map2_P0_t3T = undefined

map2_P0_t4 = Specific 4 (mkP 4 True) (7,'D') $ fromString
    " , ,_, , , ,.P4. D00, \n\
    \S, , , , ,S,   S,S\n\
    \S,S, , , ,S,   S,S\n\
    \S,S,S,S,S,S,   S,S"
;
map2_P0_t4T = undefined

-- TODO: add transition states inbetween

map2_initGS :: GameState
map2_initGS =
  either error id $ initGS (S.singleton map2_P0_t0)

map2_GS :: GameState
map2_GS = GS (M.fromList
  [(0,(f map2_P0_t0,e map2_P0_t0T)),
   (1,(f map2_P0_t1,e map2_P0_t1T)),
   (2,(f map2_P0_t2,e map2_P0_t2T)),
   (3,(f map2_P0_t3,e map2_P0_t3T)),
   (4,(f map2_P0_t4,e map2_P0_t4T))]) undefined
  where f = S.singleton
        e = const S.empty


main = do
  mapM_ (print >=> const (putStrLn "")) $ [map1_P0, map1_P1]
  putStrLn "Initial Gamestate:"
  print map2_GS
;

-- TODO: implement basic example game state