module Main where

import qualified Lexer as L
import qualified Parser as P
import qualified Runtime as R

main :: IO ()
main = do
  bf <- getContents
  d <- either printErrors run . P.parse . L.tokenize $ bf
  putStrLn d
  where
    zeroData = R.initData 0
    printErrors x = return . concat . fmap show $ x
    run = fmap show . R.runProgram zeroData . P.optimize 
