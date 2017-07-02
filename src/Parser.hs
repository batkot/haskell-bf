module Parser 
  ( parse
  , optimize
  , Command (..)
  ) where

import qualified Lexer as L
import Data.Bifunctor 
import Data.Either (lefts, rights)

data Command = Move Int
             | Add Int
             | Input
             | Output 
             | NoOp
             | Loop [Command]
             deriving (Show, Eq)

data SyntaxErrorType = MissingLoopOpening
                     | MissingLoopClose deriving (Show, Eq)

data SyntaxError = SyntaxError 
  { token :: L.Token
  , errorType :: SyntaxErrorType } deriving (Show, Eq)

data ParsingContext = Ctx
  { openBrackets :: [L.Token]
  , commands :: [Command]
  , errors :: [SyntaxError] }

flattenEither :: [Either a b] -> Either [a] [b]
flattenEither e = case lefts e of
  [] -> Right $ rights e
  x -> Left x

parseToken :: L.Token -> [L.Token] -> (Either [SyntaxError] Command, [L.Token])
parseToken (L.Token L.MoveLeft _) cs = (Right (Move (-1)), cs)
parseToken (L.Token L.MoveRight _) cs = (Right (Move 1), cs)
parseToken (L.Token L.Increment _) cs = (Right (Add 1), cs)
parseToken (L.Token L.Decrement _) cs = (Right (Add (-1)), cs)
parseToken (L.Token L.Print _) cs =  (Right Output, cs)
parseToken (L.Token L.Read _) cs =  (Right Input, cs)
parseToken (L.Token (L.Comment _) _) cs = (Right NoOp, cs)
parseToken startToken@(L.Token L.StartLoop _) cs = parseLoop cs []
  where
    parseLoop :: [L.Token] -> [Command] -> (Either [SyntaxError] Command, [L.Token])
    parseLoop ((L.Token L.EndLoop _):rem) done = (Right (Loop (reverse done)), rem)
    parseLoop [] _ = (Left [SyntaxError startToken MissingLoopClose], [])
    parseLoop (t:ts) acc = 
      case res of
           Left x -> (Left ((SyntaxError startToken MissingLoopClose):x), rem)
           Right cmd -> step cmd
      where
        (res, rem) = parseToken t ts
        step cmd = parseLoop rem (cmd:acc)
parseToken t@(L.Token L.EndLoop _) cs = (Left [SyntaxError t MissingLoopOpening], cs)

parse' :: [Either [SyntaxError] Command] -> [L.Token] -> [Either [SyntaxError] Command]
parse' acc [] = reverse acc
parse' acc (t:ts) = parse' (res:acc) rem
  where
    (res, rem) = parseToken t ts

parse :: [L.Token] -> Either [SyntaxError] [Command]
parse = first concat . flattenEither . parse' []

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
