{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module ParserSpecs
  (tests
  ) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Parser as P
import qualified Lexer as L
import Arbitrary

tests :: Test
tests = testGroup "Parser tests" [
    testProperty "Tokens in Loop should create the same program" prop_Loop,
    testProperty "Optimized program shouldn't be longer than source" prop_optimizedProgramShouldBeShorterOrEqual,
    testProperty "Optimize should squash move commands to one" prop_optimizeSquashesMoveCommands,
    testProperty "Optimize should squash add commands to one" prop_optimizeSquashesAddCommands,
    testProperty "Optimized loop contains optimized commands" prop_optimizedLoopContainsOptimizedCommands,
    testProperty "Non closed bracket results in SyntaxError" prop_nonClosedBracketResultsInSyntaxError,
    testProperty "Not opened bracket results in SyntaxError" prop_notOpenedBracketResultsInSyntaxError
  ]

prop_Loop :: [L.Token] -> Bool
prop_Loop input = 
  P.parse input == (fmap extractProgramInLoop . P.parse . wrapInLoop $ input)
  where 
    createToken x = L.Token { L.tokenType = x, L.position = (L.Position 0 0) }
    wrapInLoop i = [createToken L.StartLoop]
                       ++ i 
                       ++ [createToken L.EndLoop]
    extractProgramInLoop ((P.Loop p):[])  = p
    extractProgramInLoop x = x

prop_optimizedProgramShouldBeShorterOrEqual :: [P.Command] -> Bool
prop_optimizedProgramShouldBeShorterOrEqual cmd = 
  length cmd >= (length optimized)
    where 
      optimized = P.optimize cmd

prop_optimizeSquashesMoveCommands :: [Int] -> Bool
prop_optimizeSquashesMoveCommands x =
  case optimized of
    [] -> 0 == sum x
    ((P.Move r):_) -> r == sum x
    _ -> False
  where
    optimized = P.optimize . fmap P.Move $ x

prop_optimizeSquashesAddCommands :: [Int] -> Bool
prop_optimizeSquashesAddCommands x =
  case optimized of
    [] -> 0 == sum x
    ((P.Add r):_) -> r == sum x
    _ -> False
  where
    optimized = P.optimize . fmap P.Add $ x

prop_optimizedLoopContainsOptimizedCommands :: [P.Command] -> Bool
prop_optimizedLoopContainsOptimizedCommands cmds = 
  P.optimize cmds == (deLoop . head . P.optimize $ [inLoop cmds])
  where
    inLoop = P.Loop 
    deLoop (P.Loop x) = x
    deLoop x = [x]

prop_nonClosedBracketResultsInSyntaxError :: [L.Token] -> Bool
prop_nonClosedBracketResultsInSyntaxError tokens = 
  either matchErrorType (\_ -> False) . P.parse $ withOpenLoop
  where
    matchErrorType (P.SyntaxError{P.errorType = et}:[])  = et == P.MissingLoopClose
    matchErrorType _ = False
    withOpenLoop = (L.Token L.StartLoop (L.Position 0 0 )):tokens

prop_notOpenedBracketResultsInSyntaxError :: [L.Token] -> Bool
prop_notOpenedBracketResultsInSyntaxError tokens = 
  either matchErrorType (\_ -> False) . P.parse $ withClosedLoop
  where
    matchErrorType (P.SyntaxError{P.errorType = et}:[]) = et == P.MissingLoopOpening
    matchErrorType _ = False
    withClosedLoop = (L.Token L.EndLoop (L.Position 0 0)):tokens
