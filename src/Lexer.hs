module Lexer
    ( tokenize
    , untokenize
    , Token(..)
    , TokenType(..)
    , Position(..)
    ) where

data Position = Position 
  { line :: Int
  , col :: Int } deriving (Show, Eq)

zeroPosition = Position 0 0

data Token = Token 
  { tokenType :: TokenType
  , position :: Position } deriving (Show, Eq)

data TokenType = MoveLeft
               | MoveRight
               | Increment
               | Decrement
               | Print
               | Read
               | StartLoop
               | EndLoop
               | Comment Char deriving (Show, Eq)

parseTokenType :: Char -> TokenType
parseTokenType '<' = MoveLeft
parseTokenType '>' = MoveRight
parseTokenType '+' = Increment
parseTokenType '-' = Decrement
parseTokenType '.' = Print
parseTokenType ',' = Read
parseTokenType '[' = StartLoop
parseTokenType ']' = EndLoop
parseTokenType x = Comment x

toBrainFuckChar :: TokenType -> Char
toBrainFuckChar MoveLeft = '<'
toBrainFuckChar MoveRight = '>'
toBrainFuckChar Increment = '+'
toBrainFuckChar Decrement = '-'
toBrainFuckChar Print = '.'
toBrainFuckChar Read = ','
toBrainFuckChar StartLoop = '['
toBrainFuckChar EndLoop = ']'
toBrainFuckChar (Comment x) = x

createToken :: Position -> Char -> Token
createToken p x = Token { tokenType = parseTokenType x, position = p }

tokenize :: String -> [Token]
tokenize = parse [] zeroPosition 
  where 
    newPos :: Char -> Position -> Position
    newPos '\n' p@Position{ line = prevLine } = p { line = prevLine + 1, col = 1 }
    newPos _ p@Position{col = prevCol } = p { col = prevCol + 1 }
    parse :: [Token] -> Position -> String -> [Token]
    parse res pos [] = reverse res
    parse res pos (c:cs) = 
      parse (token:res) p cs
      where 
        p = newPos c pos
        token = createToken p c

untokenize :: [Token] -> String
untokenize = fmap toCode
  where
    toCode = toBrainFuckChar . tokenType 
