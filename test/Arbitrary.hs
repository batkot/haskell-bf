module Arbitrary
  where

import qualified Lexer as L
import qualified Parser as P

import Test.QuickCheck

nonLoopingTokenTypeGen :: Gen L.TokenType
nonLoopingTokenTypeGen = elements [
  L.MoveLeft,
  L.MoveRight,
  L.Increment,
  L.Decrement,
  L.Print,
  L.Read,
  L.Comment ' ' ]

instance Arbitrary L.TokenType where 
  arbitrary = nonLoopingTokenTypeGen

instance Arbitrary L.Token where
  arbitrary = L.Token <$> nonLoopingTokenTypeGen <*> (pure $ L.Position 0 0)

nonLoopingCommandGen :: Gen P.Command
nonLoopingCommandGen = oneof [
      P.Move <$> arbitrary,
      P.Add <$> arbitrary,
      pure $ P.Input,
      pure $ P.Output
  ]

instance Arbitrary P.Command where
  arbitrary = oneof [
      nonLoopingCommandGen,
      P.Loop <$> listOf nonLoopingCommandGen
    ]
