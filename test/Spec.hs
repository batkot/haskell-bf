import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import qualified LexerSpecs as L
import qualified ParserSpecs as P

main :: IO ()
main = defaultMain tests

tests = [ L.tests
        , P.tests]

prop1 :: Bool -> Bool
prop1 b = b == False
  where types = (b :: Bool)

prop2 :: Int -> Bool
prop2 i = i < 42
  where types = (i :: Int)

