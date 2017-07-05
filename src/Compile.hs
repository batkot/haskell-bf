module Compile
  ( compile
  , C(..))
  where

import qualified Parser as P

type Source = [String]

class Compilator a where
  header :: a -> Source
  footer :: a -> Source
  run :: a -> P.Command -> Source

compile :: (Compilator a) => a -> [P.Command] -> Source
compile c bf = 
  header c 
  ++ (foldl compileStep [] bf)
  ++ footer c
  where 
    compileStep acc cmd =  acc ++ run c cmd 

data C = C

instance Compilator C where
  header _ = [ "#include <stdio.h>"
             , "int main(int argc, const char * argv[])"
             , "{"
             , "  unsigned short data[3000];"
             , "  unsigned short ptr = 3000;"
             , "  while(--ptr) { data[ptr] = 0; }"
             , "  ptr = 1500; "]

  footer _ = [ "}" ]

  run _ (P.Move x) = ["ptr += " ++ show x ++ ";"]
  run _ (P.Add x) = ["data[ptr] += " ++ show x ++ ";"]
  run _ (P.Input) = ["data[ptr] = (unsigned short)getchar();"]
  run _ (P.Output) = ["putchar(data[ptr]);"]
  run _ (P.NoOp) = []
  run c (P.Loop cmds) = 
    ["while(data[ptr])", "{"]
    ++ (foldl step [] cmds)
    ++ ["}"]
      where
        step acc cmd = acc ++ run c cmd
