module KindLang.Parser.State where

import Data.Functor.Identity
import Text.Parsec
    
type ParseState = ()
type ParseMonad = Identity
    
type Parser r = ParsecT String ParseState ParseMonad r
    
