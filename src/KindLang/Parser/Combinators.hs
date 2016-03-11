module KindLang.Parser.Combinators where

import Text.Parsec

(</>) :: ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m (Either a b)
left </> right = fmap Left left <|> fmap Right right

_whitespace_ :: Parsec String u ()
_whitespace_ = optional spaces

withtws :: Parsec String u r -> Parsec String u r
withtws p = p <* _whitespace_
withlws :: Parsec String u r -> Parsec String u r
withlws p = _whitespace_ *> p
withws :: Parsec String u r -> Parsec String u r
withws = withtws . withlws

