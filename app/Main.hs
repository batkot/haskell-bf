module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (sequence)

import qualified Lexer as L
import qualified Parser as P
import qualified Runtime as R
import qualified Compile as C

data BfcOptions = BfcOptions 
                { bfcCommand :: BfcCommand
                } deriving Show

data BfcCommand = Compile CompileOptions
                | Run RunOptions
                deriving Show

data CompileOptions = CompileOptions
                    { filesToCompile :: [String]
                    , output :: String } deriving Show

data RunOptions = RunOptions
                { files :: [String]
                , mode :: RunMode } deriving Show

data RunMode = RunMode
             | ReplMode 
             deriving Show

bfcOptionsParser :: Parser BfcOptions
bfcOptionsParser = BfcOptions 
                <$> subparser
                    ( command "compile" (info compileParser ( progDesc "Compiles Brainfuck sources to given language" ))
                   <> command "run" (info runCmdParser ( progDesc "Runs Brainfuck sources" ))
                    )

compileParser :: Parser BfcCommand
compileParser = Compile <$> compileOptionsParser

compileOptionsParser :: Parser CompileOptions
compileOptionsParser = CompileOptions
                    <$> some (argument str
                               (metavar "INPUT_SOURCES"
                               <> help "Brainfuck sources to compile")
                             )
                    <*> strOption
                        ( long "output"
                        <> metavar "OUTPUT_FILE"
                        <> showDefault
                        <> value "out"
                        <> help "Output file name")

runCmdParser :: Parser BfcCommand
runCmdParser = Run <$> runOptionsParser

runOptionsParser :: Parser RunOptions
runOptionsParser = RunOptions
                <$> many (argument str
                          (metavar "INPUT_SOURCES"
                          <> help "Brainfuck sources to be run")
                         )
                <*> flag RunMode ReplMode
                    ( long "interactive"
                    <> short 'i'
                    <> help "starts REPL"
                    )

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

run' :: RunOptions -> IO()
run' (RunOptions fs _) = do
    contents <- sequence . fmap readFile $ fs
    putStrLn . concat . fmap show . fmap (P.parse . L.tokenize ) $ contents

compile' :: CompileOptions -> IO()
compile' _ = putStrLn "Hello from compile"

main :: IO()
main = execParser opts >>= dispatch
  where
    opts = info (bfcOptionsParser <**> helper)
      ( fullDesc
      <> progDesc "Description of this sheeeeit"
      <> header "The _ Brainfuck Compilation System")
    dispatch (BfcOptions (Run r)) = run' r
    dispatch (BfcOptions (Compile c)) = compile' c
