module Lexer
    ( tokenize
    , untokenize
    , Token(..)
    ) where

data Token = MoveLeft
           | MoveRight
           | Increment
           | Decrement
           | Print
           | Read
           | StartLoop
           | EndLoop
           | Comment Char

parseToken :: Char -> Token
parseToken '<' = MoveLeft
parseToken '>' = MoveRight
parseToken '+' = Increment
parseToken '-' = Decrement
parseToken '.' = Print
parseToken ',' = Read
parseToken '[' = StartLoop
parseToken ']' = EndLoop
parseToken x = Comment x

toBrainFuckCode :: Token -> Char
toBrainFuckCode MoveLeft = '<'
toBrainFuckCode MoveRight = '>'
toBrainFuckCode Increment = '+'
toBrainFuckCode Decrement = '-'
toBrainFuckCode Print = '.'
toBrainFuckCode Read = ','
toBrainFuckCode StartLoop = '['
toBrainFuckCode EndLoop = ']'
toBrainFuckCode (Comment x) = x

tokenize :: String -> [Token]
tokenize = fmap parseToken

untokenize :: [Token] -> String
untokenize = fmap toBrainFuckCode
