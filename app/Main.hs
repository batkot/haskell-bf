module Main where

import qualified Lexer as L
import qualified Parser as P
import qualified Runtime as R
import qualified Compile as C

cCompile :: IO()
cCompile = compile . C.transpileToC $ C.C "ptr" "data"

compile :: C.Compilator -> IO()
compile compilator = getContents >>= runCompilation
  where
    runCompilation = mapM_ putStrLn . compilationPipeline compilator

compilationPipeline :: C.Compilator -> String -> [String]
compilationPipeline compilator bf = either printErrors doCompile . P.parse . L.tokenize $ bf
  where
    printErrors = fmap show
    doCompile = compilator . P.optimize

run :: IO()
run = do
  bf <- getLine
  d <- either printErrors doRun . P.parse . L.tokenize $ bf
  putStrLn d
  main
  where
    zeroData = R.initData 0
    printErrors x = return . concat . fmap show $ x
    doRun = fmap show . R.runProgram zeroData . P.optimize 

main :: IO()
main = cCompile
