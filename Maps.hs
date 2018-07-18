{-# LANGUAGE RankNTypes, ImpredicativeTypes, ExistentialQuantification, FlexibleContexts #-}

module Maps
  where

import Base
import View
import GameState
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad

map1_P0 = PW True $ Specific 0 "P" 0 $ Sized { size = (7,'D'),
  mapping =
    fromString $ 
    ".P0., ,_, , ,tx1 tx0, D00, \n\
    \   S, , , , ,      S,   S,S\n\
    \   S,S, , , ,      S,   S,S\n\
    \   S,S,S,S,S,      S,   S,S"
}

map1_P1 = PW False $ Specific 1 "P" 1 $ Sized { size = (7,'D'),
  mapping = fromString $
    " , ,_, , , , .<P1>(k). tx0 tx1 D00, \n\
    \S, , , , , S,   S,S\n\
    \S,S, , , , S,   S,S\n\
    \S,S,S,S,S, S,   S,S"
}

map2_P0_t0 = PW True $ Specific 0 "P" 0 $ Sized { size = (7,'D'),
  mapping =
    fromString $ 
    ".P0., ,tx1 _, , ,tx0, D00, \n\
    \   S, ,     , , ,  S,   S,S\n\
    \   S,S,     , , ,  S,   S,S\n\
    \   S,S,    S,S,S,  S,   S,S"
}
map2_P0_t0T = undefined

map2_P0_t1 = PW True $ Specific 1 "P" 1 $ Sized { size = (7,'D'),
  mapping =
    fromString $ 
    " ,    ,tx1 _, , ,tx0, D00, \n\
    \S,.P1.,     , , ,  S,   S,S\n\
    \S,   S,     , , ,  S,   S,S\n\
    \S,   S,    S,S,S,  S,   S,S"
}
map2_P0_t1T = undefined

map2_P0_t2 = PW True $ Specific 2 "P" 2 $ Sized { size = (7,'D'),
  mapping =
    fromString $ 
    " , ,.P2. tx1 _, , ,tx0, D00, \n\
    \S, ,     , , ,  S,   S,S\n\
    \S,S,     , , ,  S,   S,S\n\
    \S,S,    S,S,S,  S,   S,S"
}
map2_P0_t2T = undefined

map2_P0_t3 = PW True $ Specific 3 "P" 3 $ Sized { size = (7,'D'),
  mapping =
    fromString $
    " , ,_, , ,.P3., D00, \n\
    \S, , , , ,   S,   S,S\n\
    \S,S, , , ,   S,   S,S\n\
    \S,S,S,S,S,   S,   S,S"
}
map2_P0_t3T = undefined

map2_P0_t4 = PW True $ Specific 4 "P" 4 $ Sized { size = (7,'D'),
  mapping =
    fromString $ 
    " , ,_, , , ,.P4. D00, \n\
    \S, , , , ,S,   S,S\n\
    \S,S, , , ,S,   S,S\n\
    \S,S,S,S,S,S,   S,S"
}
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