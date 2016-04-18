module KindLang.Data.BasicTypes where

import Data.Maybe
import qualified Data.Map as Map
import KindLang.Util.Control
    
data NSID = 
     UnqualifiedID String |
     QualifiedID String NSID
     deriving (Eq, Ord)

type Identified o = (NSID, o)
type IdentMap o = Map.Map String (Identified o)
    
qualifiedBy :: NSID -> NSID -> NSID
i `qualifiedBy` (UnqualifiedID s) = QualifiedID s i
i `qualifiedBy` (QualifiedID s s') = QualifiedID s (i `qualifiedBy` s')

qualifiedByStrings :: String -> [String] -> NSID
qualifiedByStrings s q = foldr QualifiedID (UnqualifiedID s) q
                       
qualifierOf :: NSID -> Maybe NSID
qualifierOf (UnqualifiedID _) = Nothing
qualifierOf (QualifiedID s (UnqualifiedID _)) = Just $ UnqualifiedID s
qualifierOf (QualifiedID s s') = Just (QualifiedID s
                                       (fromJust $ qualifierOf s'))
unscopedIdOf :: NSID -> String
unscopedIdOf (UnqualifiedID s) = s
unscopedIdOf (QualifiedID _ s') = unscopedIdOf s'

scopedIdToList :: NSID -> [String]
scopedIdToList (UnqualifiedID s) = [s]
scopedIdToList (QualifiedID s s') = s:(scopedIdToList s')

listToNSID :: [String] -> NSID
listToNSID = foldrn QualifiedID UnqualifiedID
                 
scopedIDString :: NSID -> String
scopedIDString = foldl1 (\ a b -> a ++ "::" ++ b) . scopedIdToList
                 
instance Show NSID where
    show s = "(NSID " ++ show (scopedIDString s) ++ ")"
