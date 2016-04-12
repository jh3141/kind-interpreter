module KindLang.Data.BasicTypes where

import Data.Maybe
import qualified Data.Map as Map
import KindLang.Util.Control
    
data ScopedID = 
     UnqualifiedID String |
     QualifiedID String ScopedID
     deriving (Eq, Ord)

type Identified o = (ScopedID, o)
type IdentMap o = Map.Map String (Identified o)
    
qualifiedBy :: ScopedID -> ScopedID -> ScopedID
i `qualifiedBy` (UnqualifiedID s) = QualifiedID s i
i `qualifiedBy` (QualifiedID s s') = QualifiedID s (i `qualifiedBy` s')

qualifiedByStrings :: String -> [String] -> ScopedID
qualifiedByStrings s q = foldr QualifiedID (UnqualifiedID s) q
                       
qualifierOf :: ScopedID -> Maybe ScopedID
qualifierOf (UnqualifiedID _) = Nothing
qualifierOf (QualifiedID s (UnqualifiedID _)) = Just $ UnqualifiedID s
qualifierOf (QualifiedID s s') = Just (QualifiedID s
                                       (fromJust $ qualifierOf s'))
unscopedIdOf :: ScopedID -> String
unscopedIdOf (UnqualifiedID s) = s
unscopedIdOf (QualifiedID _ s') = unscopedIdOf s'

scopedIdToList :: ScopedID -> [String]
scopedIdToList (UnqualifiedID s) = [s]
scopedIdToList (QualifiedID s s') = s:(scopedIdToList s')

listToScopedID :: [String] -> ScopedID
listToScopedID = foldrn QualifiedID UnqualifiedID
                 
scopedIDString :: ScopedID -> String
scopedIDString = foldl1 (\ a b -> a ++ "::" ++ b) . scopedIdToList
                 
instance Show ScopedID where
    show s = "(ScopedID " ++ show (scopedIDString s) ++ ")"
