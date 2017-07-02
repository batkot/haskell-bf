module Main where

import qualified Lexer as L
import qualified Parser as P
import qualified Runtime as R

main :: IO ()
main = do
  bf <- getContents
  d <- either printErrors run . P.parse . L.tokenize $ bf
  return ()
  where
    zeroData = R.initData 0
    printErrors x = return zeroData
    run = R.runProgram zeroData . P.optimize 
