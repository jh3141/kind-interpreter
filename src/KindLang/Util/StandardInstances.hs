module KindLang.Util.StandardInstances where

-- instances for types that are not locally defined

instance Show (a -> b) where
    show f = "(function)"
