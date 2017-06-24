module LexerSpecs
  (tests
  ) where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Data.List

import Lexer 

tests = testGroup "Parsing Tests" [
          testProperty "Tokenization should preserve length"  prop_tokenizationLength,
          testProperty "Untokenization should revert tokenization"  prop_tokenizationLength,
          testProperty "Non Brain fuck chars should be comment" prop_nonBrainfuckCharsComments,
          testProperty "Last elements in lines should add to tokens count" prop_lastPositionLength,
          testProperty "Max line # should be equal to number of new lines" prop_numberOfLinesShouldBeEqualToNumberOfNewLines
  ]

prop_tokenizationLength :: String -> Bool
prop_tokenizationLength source = length source == length (tokenize source)

prop_tokenizationUntokenizationReverts :: String -> Bool
prop_tokenizationUntokenizationReverts source = source == roundTrip source
  where roundTrip = untokenize . tokenize

prop_lastPositionLength :: String -> Bool
prop_lastPositionLength source = length source == count source
  where 
    count = sum
          . fmap maximum
          . fmap (fmap col)
          . groupBy (\x y -> line x == line y)
          . fmap position
          . tokenize

prop_numberOfLinesShouldBeEqualToNumberOfNewLines :: String -> Bool
prop_numberOfLinesShouldBeEqualToNumberOfNewLines source = 
  countNewLines source == maxLine source
  where 
    countNewLines = length . filter (\x -> x == '\n')
    saveMax [] = 0
    saveMax x = maximum x
    maxLine = saveMax . fmap (line . position) . tokenize

prop_nonBrainfuckCharsComments :: String -> Bool
prop_nonBrainfuckCharsComments s = all (isComment . tokenType) $ tokenize nonBfString
  where 
    cont = flip elem
    nonBfString = filter (not . cont brainfuckChars) $ s
    isComment :: TokenType -> Bool
    isComment (Comment _) = True
    isComment _ = False

brainfuckChars :: [Char]
brainfuckChars = [ '>', '<', '+', '-', '.', ',', '[', ']' ] 
