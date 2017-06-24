module LexerSpecs
  (tests
  ) where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Data.List

import Lexer (tokenize, untokenize, Token(Comment))

tests = testGroup "Parsing Tests" [
          testProperty "Tokenization should preserve length"  prop_tokenizationLength,
          testProperty "Untokenization should revert tokenization"  prop_tokenizationLength,
          testProperty "Non Brain fuck chars should be comment" prop_nonBrainfuckCharsComments
  ]

prop_tokenizationLength :: String -> Bool
prop_tokenizationLength source = length source == length (tokenize source)

prop_tokenizationUntokenizationReverts :: String -> Bool
prop_tokenizationUntokenizationReverts source = source == roundTrip source
  where roundTrip = untokenize . tokenize

prop_nonBrainfuckCharsComments :: String -> Bool
prop_nonBrainfuckCharsComments s = all isComment $ tokenize nonBfString
  where 
    cont = flip elem
    nonBfString = filter (not . cont brainfuckChars) $ s
    isComment :: Token -> Bool
    isComment (Comment _) = True
    isComment _ = False

brainfuckChars :: [Char]
brainfuckChars = [ '>', '<', '+', '-', '.', ',', '[', ']' ] 
