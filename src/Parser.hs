module Parser 
  ( parse
  , Command (..)
  ) where

import qualified Lexer as L

data Command = Move Int
             | Add Int
             | Input
             | Output 
             | Loop [Command]
             deriving (Show, Eq)

type Program = [Command]

data SyntaxErrorType = MissingLoopOpening
                     | MissingLoopClose deriving (Show, Eq)

data SyntaxError = SyntaxError 
  { token :: L.Token
  , errorType :: SyntaxErrorType } deriving (Show, Eq)

parse :: [L.Token] -> Either [SyntaxError] Program
parse _ = Right $ [Loop []]
