module Main where

import qualified Lexer as L
import qualified Parser as P
import qualified Runtime as R
import qualified Compile as C

cCompile :: IO()
cCompile = compile . C.transpileToC $ C.C "ptr" "data"

compile :: C.Compilator -> IO()
compile compilator = getContents >>= run
  where
    run = mapM_ putStrLn . compilationPipeline compilator

compilationPipeline :: C.Compilator -> String -> [String]
compilationPipeline compilator bf = either printErrors compile . P.parse . L.tokenize $ bf
  where
    printErrors = fmap show
    compile = compilator . P.optimize

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
