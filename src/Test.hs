{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Base
import View()
import Control.Applicative (liftA2,liftA3)
import qualified Data.MultiSet as MS
import Data.MultiSet (MultiSet)
import Data.Char (toUpper)

-- QuickCheck overview: http://www.cse.chalmers.se/~rjmh/QuickCheck/manual_body.html#16

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

nameStrGen = elements [1..5] >>= \n -> vectorOf n (oneof [pure id, pure toUpper] <*> elements ['a'..'h'])

playerGen :: Gen Player
playerGen =
  liftA3 Player nameStrGen arbitrary arbitrary

prop_read_show_Dir :: Dir -> Bool
prop_read_show_Dir d = d == read (show d)

prop_read_show_PhyObj :: PhyObj ->Bool
prop_read_show_PhyObj d = d == read (show d)

prop_read_show_Player :: Property
prop_read_show_Player = forAll playerGen $ \d -> d == read (show d)


return []
main = $(quickCheckAll) >> return ()
