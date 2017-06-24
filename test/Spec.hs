import Test.Framework (defaultMain)

import qualified LexerSpecs as L
import qualified ParserSpecs as P

main :: IO ()
main = defaultMain tests

tests = [ L.tests
        , P.tests]
