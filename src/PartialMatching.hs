{-# LANGUAGE RankNTypes,GADTs,FlexibleInstances #-}
{-# OPTIONS -Wall #-}
module PartialMatching where

-- a module to experiment to find agood abstraction
-- for inference and condition-checking for Telegame.

import Data.Maybe

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




{- Monoid a, with
  a + b implying that
  positive things in a or b imply positive things in a+b.
  e.g. (++) works, because x in xs or x in ys implies x in xs ++ ys
  (*) doesn't work, because x <= a or x <= b, doesn't
  imply x <= a*b. e.g. 1 <= 1 but 1 > 1*(-1) == -1
-}

failWith :: r -> P c r a
failWith = Fail

-- L1
and :: (Monoid a, Eq c) => P c Res a -> P c Res a -> P c Res a
and (P f x rx) (P g y ry) =
  if (f least /= g least) then Fail "context mismatch"
    else case x `combine` y of
            Nothing -> Fail $ "contradicting stories: " ++ show x ++ " vs " ++ show y
            Just  z -> P f z (rx++ry)
;

class Show a => Cnstr a where
  least :: a
  combine :: a -> a -> Maybe a
  -- tries to combine both, and captures boths conditions.
  -- if fails, then return Nothing.
;
{- data-structure instance:
we need a and f a to have:
zero :: a
compare :: a -> a -> {lessAssuming, equallyAssuming,
moreAssuming, Incompatible}
-}
type Res = String

-- but how shall a minimal example be taken,
-- if we allow for choice?
-- default: use first one introduced.
-- or specify importancy by type-level index.

-- Nesting.
ontopof :: (c -> c') -> P c r a -> P c' r a
ontopof f x = case x of
  P g x r -> P (f.g) x r
  And p1 p2 -> And (ontopof f p1) (ontopof f p1)
  Or  p1 p2 -> Or  (ontopof f p1) (ontopof f p1)
;

-- e.g. A String <constrained> :: Sample
ex1 :: P Sample Res Int
ex1 = undefined
-- or  B [?,?,?]   :: Sample ; constraint lies in the number of elements
ex2 :: P Sample Res [Maybe C]
ex2 = undefined

-- M
cmpP :: P c r a -> P c r a -> [r]
cmpP = undefined
{- tests, that c's match and then returns the combined result on a's.
incompatible, if c's mismatch. -}

lift :: Cnstr a =>  c -> P c r a
lift c = P (const c) least []

exact :: P c r a -> c -> r
exact = undefined {- reduceable to cmpP -}

satisfiedBy :: P c r a -> f a -> r
satisfiedBy = undefined
{- reduceable to cmpP -}

-- C
-- specifically for the minimal inference
constructMinimal :: P c r a -> c
constructMinimal = undefined {- use type-class to fill in the gap. -}

-- construct :: p f a -> (f a -> a) {- context sensitive filling -} -> a
-- construct = undefined
data Id a = Id { runId :: a } deriving (Show)
{-
data Compose f g a = Compose (f (g a))
-- the datatype representing partial knowledge
data P
      f -- the context of that type. (e.g. Maybe, PlayerP etc.)
      r -- return value on constraint checking. should be a Monoid
      a -- the type that is constrained
 where
  P0 :: Cnstr a => a -> r -> P Id r a {- a normal partially specified value -}
  Pn :: Cnstr a => P g r a -> -> P (Compose f g) r a
  {- partially specified value in a composed context -}
;-}

data P c r a where
  {- backtransformation, constrained value, contradiction if failed -}
  P :: Cnstr a => (a -> c) -> a -> r -> P c r a
  And :: P c r a -> P c r a -> P c r a
  Or  :: P c r a -> P c r a -> P c r a
--
--  Fail :: r -> P c r a. fail only comes during evaluation.
-- use Data.Typeable and co to do such things.

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
t0 = P id [(3,4)] ["should be 3 -> 4"]

data Bla = D String Map
t1 :: P Bla Res Map
t1 = D "known" `ontopof` t0

{-
data SampleA a b = SampleA a b
ex1 :: P (SampleA String) Res Int
ex1 = undefined
-- or  B [?,?,?]   :: Sample ; constraint lies in the number of elements
ex2 :: P (SampleA {- lazy -} ()) Res [Maybe C]
ex2 = undefined
-}
