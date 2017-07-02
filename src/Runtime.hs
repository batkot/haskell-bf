module Runtime
  where

import qualified Parser as P

import Control.Monad (foldM)
import Data.Char (ord,chr)

data Data a = D [a] a [a] deriving Show

initData :: a -> Data a
initData a = D [] a []

moveLeft :: a -> Data a -> Data a
moveLeft d (D [] a r) = D [] d (a:r) 
moveLeft _ (D (l:ls) a r) = D ls l (a:r)

moveRight :: a -> Data a -> Data a
moveRight d (D l a []) = D (a:l) d []
moveRight d (D l a (r:rs)) = D (a:l) r rs

modify :: Data a ->  a -> Data a
modify (D l c r) x = D l x r

type ProgramData = Data Int

runCommand :: ProgramData  -> P.Command -> IO (ProgramData)
runCommand d@(D _ c _) (P.Add x) = return $ modify d $ c + x
runCommand d (P.Move x) 
  | x > 0 = return $ iterate (moveRight 0) d !! x
  | x == 0 = return d
  | otherwise = return $ iterate (moveLeft 0) d !! (-x)
runCommand d@(D _ 0 _) (P.Loop _) = return d
runCommand d l@(P.Loop cmds) = 
  foldM runCommand d cmds >>= reRunLoop 
    where 
      reRunLoop = flip runCommand $ l
runCommand d (P.Input) = 
  fmap (modify d . ord) getChar
runCommand d@(D _ c _) (P.Output) = do 
  putChar . chr $ c
  return d
runCommand d (P.NoOp) = return d

runProgram :: ProgramData -> [P.Command] -> IO (ProgramData)
runProgram d cmd = foldM runCommand d cmd
