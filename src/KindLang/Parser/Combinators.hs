{-# LANGUAGE FlexibleContexts #-}
module KindLang.Parser.Combinators where

import Control.Applicative
import KindLang.Util.Control
import Text.Parsec (ParsecT,Stream)
import qualified Text.Parsec as P

-- | @a </> b@ parses @a@ and, if successful, returns its result
-- wrapped in 'Left'.  Otherwise, parses @b@ and returns its result wrapped
-- in 'Right'.  It is thus similar to '(<|>)' except that the return types
-- of the parsers may vary, and it is later possible to identify which
-- path was taken.
(</>) :: Alternative f =>  f a -> f b -> f (Either a b)
left </> right = fmap Left left <|> fmap Right right

-- | Definition of whitespace used by later parser combinators
whitespace_ :: Stream s m Char => ParsecT s u m ()
whitespace_ = P.optional P.spaces

-- | @withtws a@ parses @a@ followed by whitespace, then returns @a@'s result
withtws :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
withtws p = p <* whitespace_
-- | @withlws a@ parses whitespace followed by @a@, then returns @a@'s result
withlws :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
withlws p = whitespace_ *> p
-- | @withws a@ parses whitespace, then @a@, then whitespace, and returns
-- @a@'s result.
withws :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
withws = withtws . withlws

-- | Equivalent to 'sepBy1', except that if an item is followed by a
-- separator and then something that is not an item, the items parsed
-- so far are returned and the separator is left unconsumed.  This can
-- therefore be used to parse lists of items that are followed by an item
-- of a different kind.
sepBy1Lazy :: Stream s m t =>
              ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m [a]
sepBy1Lazy item sep = liftA2 (:)
                             item
                             (P.manyTill
                                 sepItem
                                 (P.notFollowedBy $ ignoreResult sepItem))
                      where sepItem = sep *> item

-- | Apply a given process and ignore the result
ignoreResult :: Applicative f =>  f a -> f ()
ignoreResult p = p *> (pure ())
                 
                 
-- | sequential merging of application results into a list, where the right
-- hand side is already a list
(<:>) :: Applicative f => f a -> f [a] -> f [a]
infixr 0 <:>         
a1 <:> a2 = liftA2 (:) a1 a2
              
-- | sequential merging of application results into a list, where the right
-- hand side is not already a list
(<::>) :: Applicative f => f a -> f a -> f [a]
infixr 0 <::>         
a1 <::> a2 = a1 <:> (fmap singleton a2)

-- | merging of two application results into a pair
(<&>) :: Applicative f => f a -> f b -> f (a,b)
infixr 0 <&>         
a1 <&> a2 = liftA2 (,) a1 a2
            
bracketedBy :: Stream s m Char =>
               Char -> Char -> ParsecT s u m a -> ParsecT s u m a
bracketedBy l r = P.between (withtws $ P.char l) (P.char r)
bracketed :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
bracketed = bracketedBy '(' ')'
braced :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
braced = bracketedBy '{' '}'
squareBracketed :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
squareBracketed = bracketedBy '[' ']'
                  
         
