module Runtime
  where

import qualified Parser as P

import Control.Monad (foldM)
import Data.Char (ord,chr)

data Data a = D [a] a [a] deriving Show

data Runtime m a = Runtime 
    { readData  :: m a
    , writeData :: a -> m () }

bfRuntime :: Runtime IO Int
bfRuntime = Runtime 
    (fmap ord getChar)
    (putChar . chr)

initData :: a -> Data a
initData a = D [] a []

moveLeft :: a -> Data a -> Data a
moveLeft d (D [] a r) = D [] d (a:r) 
moveLeft _ (D (l:ls) a r) = D ls l (a:r)

moveRight :: a -> Data a -> Data a
moveRight d (D l a []) = D (a:l) d []
moveRight _ (D l a (r:rs)) = D (a:l) r rs

modify :: Data a ->  a -> Data a
modify (D l _ r) x = D l x r

type ProgramData = Data Int

runCommand :: (Monad m)
           => Runtime m Int
           -> Data Int
           -> P.Command
           -> m (Data Int)
runCommand _ d@(D _ c _) (P.Add x) = return $ modify d $ c + x
runCommand _ d (P.Move x) 
  | x > 0 = return $ iterate (moveRight 0) d !! x
  | x == 0 = return d
  | otherwise = return $ iterate (moveLeft 0) d !! (-x)
runCommand _ d@(D _ 0 _) (P.Loop _) = return d
runCommand r d l@(P.Loop cmds) = 
  foldM (runCommand r) d cmds >>= reRunLoop 
    where 
      reRunLoop = flip (runCommand r) l
runCommand Runtime{readData = readD} d P.Input = fmap (modify d) readD
runCommand Runtime{writeData = writeD} d@(D _ c _) P.Output = do 
    writeD c
    return d
runCommand _ d P.NoOp = return d

runProgram :: (Monad m) => Runtime m Int -> ProgramData -> [P.Command] -> m ProgramData
runProgram r = foldM $ runCommand r
