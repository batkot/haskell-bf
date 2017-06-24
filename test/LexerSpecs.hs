module LexerSpecs
  (tests
  ) where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Lexer

tests = testGroup "Parsing Tests" [
          testProperty "Tokenization should preserve length"  prop_tokenizationLength,
          testProperty "Untokenization should revert tokenization"  prop_tokenizationLength
  ]

prop_tokenizationLength :: String -> Bool
prop_tokenizationLength source = length source == length (tokenize source)

prop_tokenizationUntokenizationReverts :: String -> Bool
prop_tokenizationUntokenizationReverts source = source == roundTrip source
  where roundTrip = untokenize . tokenize
