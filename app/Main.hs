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
                               <> help "Brainfuck sources to compile. Files are concated to single BF program in given order")
                             )
                    <*> strOption
                        ( long "output"
                        <> short 'o'
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

-- REPL
data ReplState = ReplState
               { programData :: R.ProgramData 
               } deriving Show

data ReplCommand = Interpret String
                 | Clean

repl :: ReplState -> ReplCommand -> IO (ReplState)
repl s Clean = return $ s { programData = R.initData 0 }
repl s@(ReplState d) (Interpret bf) = 
  case parseBrainfuck bf of
       Left e -> do 
        mapM_ (putStrLn . describeError) e
        return s
       Right p -> do 
        newState <- R.runProgram d p 
        putStrLn . show $ newState
        return s { programData = newState }

runRepl :: ReplState -> IO(ReplState)
runRepl s = do
  fmap parseCmd getLine >>= repl s >>= runRepl
    where
      parseCmd = Interpret
  
run :: RunOptions -> IO()
run _ = void $ runRepl $ ReplState (R.initData 0)

-- compilation
compile :: C.Compilator -> [String] -> [String]    
compile c sources = do
  case combineSources sources of
       Left e -> fmap describeError e
       Right p -> c . P.optimize $ p
  where
    combineSources = bimap concat concat . P.flattenEither . fmap parseBrainfuck

compile' :: C.Compilator -> CompileOptions -> IO()
compile' c (CompileOptions sourceFiles o) = do
  readFiles sourceFiles
  >>= writeFile o . unlines . compile c 

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
