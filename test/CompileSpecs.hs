{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module CompileSpecs 
  ( tests
  ) where


import Test.Framework (testGroup, Test) 
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.HUnit ((@=?), Assertion)
import Test.QuickCheck

import qualified Parser as P
import qualified Compile as C
import Arbitrary 

tests :: Test
tests = testGroup "C Compiler Tests" 
  [ testCase "Sample compilations" test_sampleCompilations
  ]

test_sampleCompilations :: Assertion
test_sampleCompilations = 
  fmap snd samplePrograms @=? fmap (cCompile . fst) samplePrograms 
  where
    cCompile = C.transpileToC cOpt

type CompilationExample = ([P.Command], [String])

samplePrograms :: [CompilationExample]
samplePrograms = 
  [ ([P.Add 5], buildProgram "d[p]+=5;")
  , ([P.Move 5], buildProgram "p+=5;")
  , ([P.Output], buildProgram "putchar(d[p]);")
  , ([P.Input], buildProgram "d[p]=(unsigned short)getchar();")
  , ([P.Loop [P.Add 5]], buildProgram "while(d[p]){d[p]+=5;}")
  ]

ptrName :: String
ptrName = "p"

dataName :: String
dataName = "d"

cOpt :: C.C
cOpt = C.C ptrName dataName

cIncludes :: [String]
cIncludes = ["#Include<stdio.h>"]

cHeader :: String
cHeader = "int main(int argc, const char * argv[]) \
             \ { \
             \  unsigned short " ++ dataName ++ "[3000]; \
             \  unsigned short " ++ ptrName ++ "=3000;\
             \  while(--"++ ptrName ++ ") { " ++ dataName ++ "[" ++ ptrName ++ "] = 0; }"
                ++ ptrName ++ " = 1500;"

buildProgram :: String -> [String]
buildProgram p = cIncludes ++ [cHeader ++ p ++ "}"]
