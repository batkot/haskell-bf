module Main where

import qualified Lexer as L
import qualified Parser as P
import qualified Runtime as R
import qualified Compile as C

cCompile :: IO()
cCompile = do
  bf <- getContents
  d <- either printErrors ccompile . P.parse . L.tokenize $ bf
  mapM_ putStrLn d
  where
    printErrors x = return . fmap show $ x
    ccompile = return . C.compile C.C . P.optimize

run :: IO()
run = do
  bf <- getLine
  d <- either printErrors run . P.parse . L.tokenize $ bf
  putStrLn d
  main
  where
    zeroData = R.initData 0
    printErrors x = return . concat . fmap show $ x
    run = fmap show . R.runProgram zeroData . P.optimize 

main :: IO()
main = cCompile
