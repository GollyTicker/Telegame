{-# LANGUAGE RankNTypes,GADTs,FlexibleInstances #-}
{-# OPTIONS -Wall #-}
module PartialMatching where

-- a module to experiment to find agood abstraction
-- for inference and condition-checking for Telegame.

import Data.Maybe
import Control.Applicative
import Control.Monad

data Sample = A String Int | B [Maybe C] deriving (Show,Eq)
data C = C Int | Q deriving (Show,Eq)

{- what we want
. L:      conjunctive and disjunctive logical connectives (Parsing?)
. Nesting parts of a data-structure can also be unknown.
           . we want to the able to partially specify a data-type
            . functor, high-kind-type-class?
. M:      semantics of exact and minimal-matching:
            . does an instance exactly match PInfo?
            . does an instance satisfy the minimal conditions of PInfo?
            . as a conclusion, it should also support lifting 'a' to 'PInfo a
. Constructability:
    .given a strategy (or more information) it should be able to reduce partially known structures into
-}

{-
and :: (Monoid a, Eq c) => P c Res a -> P c Res a -> P c Res a
and (P f x rx) (P g y ry) =
  if (f least /= g least) then Fail "context mismatch"
    else case x `combine` y of
            Nothing -> Fail $ "contradicting stories: " ++ show x ++ " vs " ++ show y
            Just  z -> P f z (rx++ry)
;
-}

type Res = String

-- M
cmpP :: P c r a -> P c r a -> r
cmpP = undefined
{- tests, that c's match and then returns the combined result on a's.
incompatible, if c's mismatch. -}

lift :: Cnstr a => c -> P c r a
lift c = Always c

exact :: P c r a -> c -> r
exact = undefined {- reduceable to cmpP -}

satisfiedBy :: P c r a -> f a -> r
satisfiedBy = undefined
{- reduceable to cmpP -}

-- C
-- specifically for the minimal inference
constructMinimal :: (a->c) -> a -> r -> c
constructMinimal = undefined {- use type-class to fill in the gap. -}

evalMergeC :: (Eq c,Cnstr a) => P c r a -> c -> Either r c
evalMergeC = undefined -- do-and positive result from right if it works

evalMergeACAR :: (Eq c,Cnstr a) => P c r a -> (a->c) -> a -> r -> Either r c
evalMergeACAR = undefined -- do-and leaf result from right if it works

class Show a => Cnstr a where
  least :: a
  combine :: a -> a -> Maybe a
  -- tries to combine both, and captures boths conditions.
  -- if fails, then return Nothing.
;

eval :: (Eq c,Cnstr a) => P c r a -> Either r c
eval p = undefined -- case p of
--  Always c -> Right c
--  P c a r  -> c a -- construct minimal example
--  And p p' -> -- fail, if p or p' fails. otherwise try to merge results
--    eval p >> eval p' >> undefined {- try merge a's -}
  {-And p (Always c)  -> evalMergeC p c
  And p (P c a r)    -> evalMergeACAR p c a r
  And p (And p2 p3) -> eval p2
  And p (Or  pa pb) -> _ -- <$> evalMergeACAR p pa <*> evalMergeACAR p pb
  -}
{-    case x `combine` y of
      Nothing -> Fail $ "contradicting stories: " ++ show x ++ " vs " ++ show y
      Just  z -> if f1 z == f2 z then Right c
                 
    if (f1 least /= f2 least) then Fail "context mismatch"
      else 
  -}
-- evaluates and and or and always.
-- on contradiction, returns first, or succeedes with a minimal construction

-- ACHIVED 50%: and and or
data P c r a where
  Always ::                                c -> P c r a
  {- makeFullyKnown-Function, minimally constrained value, contradiction if failed -}
  P   :: Cnstr a => (a -> c) -> a       -> r -> P c r a
  And ::            P c r a  -> P c r a      -> P c r a
  Or  ::            P c r a  -> P c r a      -> P c r a
;
{-
traverseP :: Monad f =>
  (c -> z -> f z)
  -> ((a->c) -> a -> r -> z -> f z)
  -> (z -> z -> z -> f z)
  -> (z -> z -> z -> f z)
  -> P c r a -> z -> f z
traverseP always p and or = go where
  go myp z = case myp of
    Always c -> always c z
    P c a r  -> p c a r z
    And p1 p2 -> go p1 z >>= \z1 -> go p1 z1 >>= \z2 -> and z1 z2 -- nesessary?
    Or  p1 p2 -> go p1 z 
;
-}
{-
foldP :: Cnstr a =>
  (c -> b)
  -> ((a -> c) -> a -> r -> b)
  -> (b -> b -> b)
  -> (b -> b -> b)
  -> P c r a -> b
foldP always p and or = go where
  go myp = case myp of
    Always c -> always c
    P c a r  -> p c a r
    And p1 p2 -> and (go p1) (go p2)
    Or  p1 p2 -> or  (go p1) (go p2)
;
-}

--  Fail :: r -> P c r a. fail only comes during evaluation.
-- ACHIVED: Nesting.
ontopof :: (c -> c') -> P c r a -> P c' r a
ontopof f x = case x of
  P g y r -> P (f.g) y r
  And p1 p2 -> And (ontopof f p1) (ontopof f p2)
  Or  p1 p2 -> Or  (ontopof f p1) (ontopof f p2)
  Always c  -> Always (f c)
;

instance Cnstr [(Int,Int)] where -- implementing consistent map union
  least = []
  combine as bs = f $ do
      (x,vx) <- as
      if any (\(y,vy) -> x==y && vx/=vy) bs
        then [Nothing]
        else [Just (x,vx)]
    where f xs = if all isJust xs then Just (map fromJust xs) else Nothing
;
-- simple constraint
type Map = [(Int,Int)]
t0 :: P Map Res Map
t0 = P id [(3,4)] "should be 3 -> 4"

data Bla = D String Map
t1 :: P Bla Res Map
t1 = D "known" `ontopof` t0

