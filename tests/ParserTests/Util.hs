module ParserTests.Util (parseString) where

import Text.Parsec
import Control.Arrow
    
parseString :: Parsec String () r -> String -> r
parseString parser toParse =
    either (show >>> error) id $ parse parser "test" toParse

    
