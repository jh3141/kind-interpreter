{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module KindLang.Util.Control where

-- | From Bird, Pearls of Functional Algorithm Design, CUP 2010
-- modified to fix compiler warnings. Like 'foldr' but uses the final
-- item of its list argument as the seed for the folding function, converted
-- via a second function.  E.g. @foldrn (+) id [1,2,3]@ is equivalent to
-- @1 + (2 + 3)@ (note that no zero element is involved as in the more common
-- @foldr (+) 0 [1,2,3]@, which evaluates to @1 + (2 + (3 + 0))@.
foldrn :: (a -> b -> b) -> (a -> b) -> [a] -> b
foldrn _ g [x]      = g x
foldrn f g (x : xs) = f x (foldrn f g xs)

-- | construct a list containing a single item
singleton :: a -> [a]
singleton item = [item]
                 
