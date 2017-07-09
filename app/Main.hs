module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (sequence, void)
import Data.Bifunctor (bimap)

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
                    , outputFile :: String } deriving Show

data RunOptions = RunOptions
                { files :: [String]
                , mode :: RunMode } deriving Show

data RunMode = RunMode
             | ReplMode 
             deriving Show

bfcOptionsParser :: Parser BfcOptions
bfcOptionsParser = BfcOptions 
                <$> subparser
                    ( command "compile" (info (compileParser <**> helper) ( progDesc "Compiles Brainfuck sources to given language" ))
                   <> command "run" (info (runCmdParser <**> helper) ( progDesc "Runs Brainfuck sources" ))
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

parseBrainfuck :: String -> Either [P.SyntaxError] [P.Command]
parseBrainfuck = P.parse . L.tokenize

run :: RunOptions -> IO()
run (RunOptions fs _) = do
    contents <- readFiles fs
    case combineSources contents of
         Left e -> mapM_ (putStrLn . show) e
         Right p -> void $ R.runProgram (R.initData 0) $ p
  where
    combineSources = bimap concat concat . P.flattenEither . fmap parseBrainfuck

compile :: C.Compilator -> [String] -> [String]    
compile c sources = do
  case combineSources sources of
       Left e -> fmap describeError e
       Right p -> c . P.optimize $ p
  where
    combineSources = bimap concat concat . P.flattenEither . fmap parseBrainfuck

compile' :: C.Compilator -> CompileOptions -> IO()
compile' c (CompileOptions sourceFiles _) = do
  sources <- readFiles sourceFiles
  mapM_ putStrLn . compile c $ sources

cCompilator :: C.Compilator
cCompilator = C.transpileToC $ C.C "p" "d"

describeError :: P.SyntaxError -> String
describeError = show

readFiles :: [String] -> IO([String])
readFiles = sequence . fmap readFile

main :: IO()
main = execParser opts >>= dispatch
  where
    opts = info (bfcOptionsParser <**> helper)
      ( fullDesc
      <> progDesc "Simple Brainfuck interpreter, compiler"
      <> header "The Meretricious Brainfuck Compilation System")
    dispatch (BfcOptions (Run r)) = run r
    dispatch (BfcOptions (Compile c)) = compile' cCompilator c
