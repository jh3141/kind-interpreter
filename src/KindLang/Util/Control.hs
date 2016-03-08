module KindLang.Util.Control where

-- foldrn, from Bird, Pearls of Functional Algorithm Design, CUP 2010
foldrn :: (a -> b -> b) -> (a -> b) -> [a] -> b
foldrn f g [x]      = g x
foldrn f g (x : xs) = f x (foldrn f g xs)
