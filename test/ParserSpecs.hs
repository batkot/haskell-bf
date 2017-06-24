module ParserSpecs
  (tests
  ) where

import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import qualified Parser as P
import qualified Lexer as L

tests = testGroup "Parser tests" [
    testProperty "Tokens in Loop should create the same program" prop_Loop,
    testProperty "Optimized program shouldn't be longer than source" prop_optimizedProgramShouldBeShorterOrEqual,
    testProperty "Optimize should squash move commands to one" prop_optimizeSquashesMoveCommands,
    testProperty "Optimize should squash add commands to one" prop_optimizeSquashesAddCommands,
    testProperty "Optimized loop contains optimized commands" prop_optimizedLoopContainsOptimizedCommands
  ]

prop_Loop :: [L.Token] -> Bool
prop_Loop input = 
  P.parse input == (fmap extractProgramInLoop . P.parse . wrapInLoop $ input)
  where 
    createToken x = L.Token { L.tokenType = x, L.position = (L.Position 0 0) }
    wrapInLoop input = [createToken L.StartLoop]
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

allCommandsGen :: Gen P.Command
allCommandsGen = elements [
    P.Move 5,
    P.Add 3,
    P.Input,
    P.Output
  ]

instance Arbitrary P.Command where
  arbitrary = oneof [ allCommandsGen ]

prop_optimizedProgramShouldBeShorterOrEqual :: [P.Command] -> Bool
prop_optimizedProgramShouldBeShorterOrEqual cmd =
  length cmd >= (length optimized)
    where 
      optimized = P.optimize cmd

prop_optimizeSquashesMoveCommands :: [Int] -> Bool
prop_optimizeSquashesMoveCommands x =
  case optimized of
    [] -> 0 == sum x
    ((P.Move r):rs) -> r == sum x
    _ -> False
  where
    optimized = P.optimize . fmap P.Move $ x

prop_optimizeSquashesAddCommands :: [Int] -> Bool
prop_optimizeSquashesAddCommands x =
  case optimized of
    [] -> 0 == sum x
    ((P.Add r):rs) -> r == sum x
    _ -> False
  where
    optimized = P.optimize . fmap P.Add $ x

prop_optimizedLoopContainsOptimizedCommands :: [P.Command] -> Bool
prop_optimizedLoopContainsOptimizedCommands cmds = 
  P.optimize cmds == (deLoop . head . P.optimize $ [inLoop cmds])
  where
    inLoop = P.Loop 
    deLoop (P.Loop x) = x

