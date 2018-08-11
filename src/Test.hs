{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.HUnit
import Base
import View()
import Control.Applicative (liftA2,liftA3)
import qualified Data.MultiSet as MS
import Data.MultiSet (MultiSet)
import Data.Char (toUpper)

-- QuickCheck overview: http://www.cse.chalmers.se/~rjmh/QuickCheck/manual_body.html#16

-- HUNIT: http://hackage.haskell.org/package/HUnit-1.6.0.0/docs/Test-HUnit.html


htests = TestList []


{- TODO:
use equalities for Partial a where.

. conjunct is associative:
    conjunct a b >>= flip conjunct c == conjunct b c >>= conjunct a
. conjunct is commutative:
    conjunct a b = conjunct b a
. concretizing a partial view of an object is id:
    Just x = (concrete . toPartial) x
. a `conjunct` b == Just x ==> isJust (merge a x)
. toPartial injective: a /= b ==> toPartial a /= toPartial b
-}

instance Arbitrary Dir where
  arbitrary = elements [L,U,R,D]
;

instance Arbitrary PhyObj where
  arbitrary = oneof [pure Key, liftA2 TOrb (elements ['a'..'e']) (elements [0,1])]
;

instance (Ord a, Arbitrary a) => Arbitrary (MultiSet a) where
  arbitrary = do
    n <- elements [0..10]
    MS.fromList <$> vector n
;

instance Arbitrary Env where
  arbitrary = do
    oneof [do n <- elements [0..4]; h <- elements [0..n]; return $ Door n h,
           return Solid,return Blank,return Platform,
           Switch <$> arbitrary]

nameStrGen = elements [1..5] >>= \n -> vectorOf n (oneof [pure id, pure toUpper] <*> elements ['a'..'h'])

playerGen :: Gen Player
playerGen =
  liftA3 Player nameStrGen arbitrary arbitrary

prop_read_show_Dir :: Dir -> Bool
prop_read_show_Dir d = d == read (show d)

prop_read_show_PhyObj :: PhyObj ->Bool
prop_read_show_PhyObj d = d == read (show d)

prop_read_show_Env :: Dir -> Bool
prop_read_show_Env e = e == read (show e)

prop_read_show_Player :: Property
prop_read_show_Player = forAll playerGen $ \d -> d == read (show d)


return []
main = putStrLn "QuickCkeck:\n"
  >> $(quickCheckAll)
  >> putStrLn "HUnit:\n"
  >> runTestTT htests >> return ()






