import Test.Framework (defaultMain, Test)

import qualified LexerSpecs as L
import qualified ParserSpecs as P
import qualified CompileSpecs as C

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ L.tests
        , P.tests
        , C.tests]
