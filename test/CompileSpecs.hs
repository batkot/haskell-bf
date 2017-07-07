module CompileSpecs 
  ( tests
  ) where


import Test.Framework (testGroup, Test) 
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.HUnit ((@=?), Assertion)

tests :: Test
tests = testGroup "C Compiler Tests" 
  [ testCase "test" test_1
  , testProperty "test_prop" test_prop
  ]

test_1 :: Assertion
test_1 = "A" @=? "A"

test_prop :: Int -> Bool
test_prop _ = True
