module KindLang.Data.Scope where

import Control.Monad.Except
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
    catchError (lookupHierarchical (scopeCat s) i)
               (deferToParent s)
    where
      deferToParent (Scope Nothing _) err = throwError err
      deferToParent (Scope (Just p) _) _  = scopeLookup p i

(|@+|) :: Scope -> (String,Definition) -> Scope
(Scope p cat) |@+| (n,d) = Scope p (cat |+| (UnqualifiedID n, d))
infixl 6 |@+|
    
