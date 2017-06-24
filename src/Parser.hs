module Parser 
  ( parse
  , optimize
  , Command (..)
  ) where

import qualified Lexer as L

data Command = Move Int
             | Add Int
             | Input
             | Output 
             | Loop [Command]
             deriving (Show, Eq)

data SyntaxErrorType = MissingLoopOpening
                     | MissingLoopClose deriving (Show, Eq)

data SyntaxError = SyntaxError 
  { token :: L.Token
  , errorType :: SyntaxErrorType } deriving (Show, Eq)

data ParsingContext = ParsingContext 
  { openBrackets :: [L.Token]
  , commands :: [Command]
  , errors :: [SyntaxError] }

parseStep :: [L.Token] -> ([Command], [L.Token])
parseStep ((L.Token L.MoveLeft _):cs) = ([Move (-1)], cs)
parseStep ((L.Token L.MoveRight _):cs) = ([Move 1], cs)
parseStep ((L.Token L.Increment _):cs) = ([Add 1], cs)
parseStep ((L.Token L.Decrement _):cs) = ([Add (-1)], cs)
parseStep ((L.Token L.Print _):cs) = ([Output], cs)
parseStep ((L.Token L.Read _):cs) = ([Input], cs)
parseStep ((L.Token (L.Comment _) _):cs) = ([], cs)
parseStep ((L.Token L.StartLoop _):cs) = parseLoop cs []
  where
    parseLoop ((L.Token L.EndLoop _):cs) done = ([Loop (reverse done)], cs)
    parseLoop x curr = parseLoop rem (curr ++ cmds)
      where
        (cmds, rem) = parseStep x 
parseStep x = ([], x)

parse' :: [Command] -> [L.Token] -> [Command]
parse' acc [] = reverse acc
parse' acc t = parse' (acc ++ cmds) rem
  where
    (cmds, rem) = parseStep t

parse :: [L.Token] -> Either [SyntaxError] [Command]
parse = Right . parse' [] 

optimizeStep :: [Command] -> [Command] -> [Command]
optimizeStep done [] = reverse done
optimizeStep acc ((Move x):(Move y):cs) = optimizeStep acc $ (Move (x + y)):cs
optimizeStep acc ((Add x):(Add y):cs) = optimizeStep acc $ (Add (x + y)):cs
optimizeStep acc ((Move 0):cs) = optimizeStep acc cs
optimizeStep acc ((Add 0):cs) = optimizeStep acc cs
optimizeStep acc ((Loop cmds):cs) = 
  optimizeStep (optimizedLoop:acc) cs
  where
    optimizedLoop = Loop $ optimizeStep [] cmds
optimizeStep acc (x:cs) = optimizeStep (x:acc) cs

optimize :: [Command] -> [Command]
optimize = optimizeStep [] 
