module ParserSpecs
  (tests
  ) where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import qualified Parser as P
import qualified Lexer as L

tests = testGroup "Parser tests" [
    testProperty "Tokens in Loop should create the same program" prop_Loop
  ]

prop_Loop :: [L.Token] -> Bool
prop_Loop input = 
  P.parse input == (fmap extractProgramInLoop . P.parse $ wrapped input)
  where 
    createToken x = L.Token { L.tokenType = x, L.position = (L.Position 0 0) }
    wrapped input = [createToken L.StartLoop]
                 ++ input 
                 ++ [createToken L.EndLoop]
    extractProgramInLoop ((P.Loop p):[])  = p
    extractProgramInLoop x = x

nonLoopingTokenTypeGen :: Gen L.TokenType
nonLoopingTokenTypeGen = elements [
  L.MoveLeft,
  L.MoveRight,
  L.Increment,
  L.Decrement,
  L.Print,
  L.Read,
  L.Comment ' ' ]

instance Arbitrary L.Token where
  arbitrary = oneof [ pure $ L.Token { L.tokenType = L.MoveLeft, L.position = L.Position 0 0 } ]
