module Compile
  ( Compilator
  , transpileToC
  , C(..))
  where

import qualified Parser as P

type Source = [String]

type Compilator = [P.Command] -> Source

data C = C 
       { ptrName :: String
       , dataName :: String
       }

dataToken :: C -> String
dataToken (C ptr d) = d ++ "[" ++ ptr ++ "]"

transpileCommand :: C -> P.Command -> String
transpileCommand (C ptr _) (P.Move x) = ptr ++ "+=" ++ show x ++ ";"
transpileCommand c (P.Add x) = dataToken c ++ "+=" ++ show x ++ ";"
transpileCommand c P.Input = dataToken c ++ "=" ++ "(unsigned short)getchar();"
transpileCommand c P.Output = "putchar(" ++ dataToken c ++ ");"
transpileCommand _ P.NoOp = []
transpileCommand c (P.Loop cmds) = "while(" ++ dataToken c ++ "){" ++ foldl step [] cmds ++ "}"
  where
    step acc cmd = acc ++ transpileCommand c cmd

transpileToC :: C -> [P.Command] -> Source
transpileToC opt cmds = 
  header ++ [body, cFooter]
  where
    header = cHeader opt
    step acc cmd = acc ++ transpileCommand opt cmd
    body = foldl step [] cmds

cHeader :: C -> [String]
cHeader (C ptr d) =  [ "#include <stdio.h>"
             , "int main(int argc, const char * argv[])"
             , "{"
             , "  unsigned short " ++ d ++ "[3000];"
             , "  unsigned short " ++ ptr ++ "=3000;"
             , "  while(--"++ ptr ++ ") { data[ptr] = 0; }"
             , "  " ++ ptr ++ " = 1500; "]

cFooter :: String
cFooter = "}"
