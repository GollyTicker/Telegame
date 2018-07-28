{-# LANGUAGE RankNTypes,GADTs #-}

module PartialMatching where

-- a module to experiment to find agood abstraction
-- for inference and condition-checking for Telegame.

data Sample = A String Int | B [Maybe C] deriving (Show,Eq)
data C = C Int | D deriving (Show,Eq)

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

-- L1
and :: P f a -> P f a -> P f a
and a b = undefined

{- data-structure instance:
we need a and f a to have:
zero :: a
compare :: a -> a -> {lessAssuming, equallyAssuming,
moreAssuming, Incompatible}
-}

orElse :: P f a -> P f a -> P f a
orElse a b = undefined
-- but how shall a minimal example be taken,
-- if we allow for choice?
-- default: use first one introduced.
-- or specify importancy by type-level index.

-- Nesting.
-- e.g. A String <constrained> :: Sample
data SampleA a b = SampleA a b
ex1 :: P (SampleA String) Int
ex1 = undefined
-- or  B [?,?,?]   :: Sample ; constraint lies in the number of elements
ex2 :: P (SampleA {- lazy -} ()) [Maybe C]
ex2 = undefined

-- M
exact :: P f a -> f a -> Bool
exact = undefined
{- if all inner ones report equallyAssuming for lifted (f a) -}

satisfiedBy :: P f a -> f a -> Bool
satisfiedBy = undefined
{- P f a is less assuming than lifted (f a) -}

-- C
-- specifically for the minimal inference
constructMinimal :: p f a -> f a
constructMinimal = undefined {- use type-class to fill in the gap. -}

-- construct :: p f a -> (f a -> a) {- context sensitive filling -} -> a
-- construct = undefined

data Id a = Id { runId :: a } deriving (Show)
data Compose f g a = Compose (f (g a))
-- the datatype representing partial knowledge
data P
      f -- the context of that type. (e.g. Maybe, PlayerP etc.)
      a -- the type that is constrained
 where
  P0 :: a -> P Id a {- a normal partially specified value -}
  Pn :: P g a -> (forall ga. f ga) -> P (Compose f g) a
  {- partially specified value in a composed context -}


