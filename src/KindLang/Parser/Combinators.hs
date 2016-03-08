module KindLang.Parser.Combinators where

import Text.Parsec

(</>) :: ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m (Either a b)
left </> right = fmap Left left <|> fmap Right right
