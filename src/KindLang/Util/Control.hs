{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module KindLang.Util.Control where

-- foldrn, from Bird, Pearls of Functional Algorithm Design, CUP 2010
-- modified to fix compiler warnings
foldrn :: (a -> b -> b) -> (a -> b) -> [a] -> b
foldrn _ g [x]      = g x
foldrn f g (x : xs) = f x (foldrn f g xs)
