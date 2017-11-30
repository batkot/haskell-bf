module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (void, foldM)
import Data.Bifunctor (bimap)
import System.IO (hFlush, stdout)

import qualified Lexer as L
import qualified Parser as P
import qualified Runtime as R
import qualified Compile as C

newtype BfcOptions = BfcOptions 
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
                               <> help "Brainfuck sources to compile. Files are parsed and compiled in given order to single BF program")
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
newtype ReplState = ReplState
               { programData :: R.ProgramData 
               } deriving Show

data ReplCommand = Interpret String
                 | Load String
                 | Clean
                 | Help

parseReplCommand :: String -> ReplCommand
parseReplCommand (":clean") = Clean
parseReplCommand (":c") = Clean
parseReplCommand (':':'l':'o':'a':'d':' ':x) = Load x
parseReplCommand (':':'l':' ':x) = Load x
parseReplCommand (':':_) = Help
parseReplCommand x = Interpret x


repl :: ReplState -> ReplCommand -> IO ReplState
repl s Help = putStrLn "Help" >>= \() -> return s
repl s Clean = return $ s { programData = R.initData 0 }
repl s (Load file) = 
  -- find some kind of readFile
  -- FileName -> IO (Maybe String)
  -- FileName -> Either Error IO(String)
  fmap Interpret (readFile file) >>= repl s 
repl s@(ReplState d) (Interpret bf) = 
  case parseBrainfuck bf of
       Left e -> mapM_ (putStrLn . describeError) e >>= \() -> return s
       Right p -> R.runProgram d p >>= \ns -> return s { programData = ns }

runRepl :: ReplState -> IO ReplState
runRepl s = 
  putStr "bfi| " >> hFlush stdout >> fmap parseReplCommand getLine 
    >>= repl s 
    >>= (\newState -> showProgramData newState >> runRepl newState)
  where
    showProgramData = print . programData
  
run :: RunOptions -> IO()
run (RunOptions fs rMode)  = do
  cmds <- fmap (fmap Interpret) . readFiles $ fs 
  state <- foldM repl (ReplState (R.initData 0)) cmds
  case rMode of
       RunMode -> return ()
       ReplMode -> void $ runRepl state

-- compilation
compile :: C.Compilator -> [String] -> Either [String] [String]
compile c = 
  bimap (fmap describeError) (c . P.optimize) . combineSources 
  where
    combineSources = bimap concat concat . P.flattenEither . fmap parseBrainfuck

compile' :: C.Compilator -> CompileOptions -> IO()
compile' c (CompileOptions sourceFiles o) = 
  readFiles sourceFiles >>= either printErrors writeSource . compile c 
    where
      printErrors = putStrLn . unlines
      writeSource = writeFile o . unlines

cCompilator :: C.Compilator
cCompilator = C.transpileToC $ C.C "p" "d"

describeError :: P.SyntaxError -> String
describeError = show

readFiles :: [String] -> IO [String]
readFiles = traverse readFile

main :: IO()
main = execParser opts >>= dispatch
  where
    opts = info (bfcOptionsParser <**> helper)
      ( fullDesc
      <> progDesc "Simple Brainfuck interpreter, compiler and REPL"
      <> header "The Meretricious Brainfuck Compilation System")
    dispatch (BfcOptions (Run r)) = run r
    dispatch (BfcOptions (Compile c)) = compile' cCompilator c
