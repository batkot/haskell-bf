module Compile
  ( Compilator
  , transpileToC
  , transpileToHaskellCommand
  , transpileToHaskell
  , C(..))
  where

import qualified Parser as P
import Data.List

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
  cInclude ["<stdio.h>"]
  ++ [header ++ body ++ cFooter]
  where
    header = cHeader opt
    step acc cmd = acc ++ transpileCommand opt cmd
    body = foldl step [] cmds

cInclude :: [String] -> [String]
cInclude = fmap $ (++) "#include"

cHeader :: C -> String
cHeader c@(C ptr d) = "int main(int argc, const char * argv[]) \
             \ { \
             \  unsigned short " ++ d ++ "[3000]; \
             \  unsigned short " ++ ptr ++ "=3000;\
             \  while(--"++ ptr ++ ") { " ++ dataToken c ++ " = 0; }"
                ++ ptr ++ " = 1500;"

cFooter :: String
cFooter = "}"

transpileToHaskellCommand :: P.Command -> String
transpileToHaskellCommand (P.Move x) = "move (" ++ show x ++ ")"
transpileToHaskellCommand (P.Add x) = "add (" ++ show x ++ ")"
transpileToHaskellCommand P.Input = "read"
transpileToHaskellCommand P.Output = "write" 
transpileToHaskellCommand (P.Loop []) = ""
transpileToHaskellCommand (P.Loop cmds) = "loop (" ++ build cmds ++ ")"
    where
        build cmd = intercalate " >=> " $ fmap transpileToHaskellCommand cmd
transpileToHaskellCommand P.NoOp = ""


transpileToHaskell :: [P.Command] -> Source
transpileToHaskell [] = emptyProgram
transpileToHaskell cmds = definitions ++ ["    " ++ intercalate " >=> " (fmap transpileToHaskellCommand cmds) ++ inputTape]

definitions :: Source
definitions = 
    [ "module Main where"
    , "import Data.Char (ord, chr)"
    , "import Control.Monad ((>=>))"
    , "data Tape a = Tape [a] a [a] deriving Show"
    , "move :: (Num a, Monad m) => Int -> Tape a -> m (Tape a)"
    , "move 0 t = return t"
    , "move x t"
    , "    | x > 0 = return $ iterate moveLeft t !! x"
    , "    |otherwise = return $ iterate moveRight t !! (-x)"
    , "        where "
    , "            moveLeft :: Num a => Tape a -> Tape a"
    , "            moveLeft (Tape [] c r) = Tape [] 0 (c:r)"
    , "            moveLeft (Tape (l:ls) c r) = Tape ls l (c:r)"
    , "            moveRight :: Num a => Tape a -> Tape a"
    , "            moveRight (Tape l c []) = Tape (c:l) 0 []"
    , "            moveRight (Tape l c (r:rs)) = Tape (c:l) r rs"
    , "set :: Monad m => a -> Tape a -> m (Tape a)"
    , "set x (Tape l _ r) = return $ Tape l x r"
    , "add :: (Num a, Monad m) => a -> Tape a -> m (Tape a)"
    , "add x t@(Tape _ c _) = set (c + x) t"
    , "read :: Tape Int -> IO (Tape Int)"
    , "read t = getChar >>= (flippedSet t . ord)"
    , "    where"
    , "        flippedSet = flip set"
    , "write :: Tape Int -> IO (Tape Int)"
    , "write t@(Tape _ v _) = do"
    , "    putChar . chr $ v"
    , "    return t"
    , "loop :: (Tape Int -> IO (Tape Int)) -> Tape Int -> IO (Tape Int)"
    , "loop _ t@(Tape _ 0 _) = return t"
    , "loop cmd t = cmd t >>= loop cmd"
    , "main :: IO()"
    , "main = "]

inputTape :: String
inputTape = " >=> const (return ()) $ Tape [] 0 []"

emptyProgram :: Source
emptyProgram = [ "module Main where"
               , "main :: IO()"
               , "main = return()"]
