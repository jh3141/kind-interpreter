module KindLang.Data.Scope where

import KindLang.Data.BasicTypes
import KindLang.Data.Catalogue
import KindLang.Data.AST
import KindLang.Data.Error
    
data Scope =
    Scope
    {
      scopeParent :: Maybe Scope,
      scopeCat :: Catalogue
    }
    deriving (Show, Eq)
             
scopeLookup :: Scope -> NSID -> KErr IdentDefinition
scopeLookup s i =
    either (deferToParent s) {- if left, or -} Right {- if right -}
           (lookupHierarchical (scopeCat s) i)
    where
      deferToParent (Scope Nothing _) err = Left err
      deferToParent (Scope (Just p) _) _  = scopeLookup p i
