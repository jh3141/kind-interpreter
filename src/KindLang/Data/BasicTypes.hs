module KindLang.Data.BasicTypes where

import Data.Maybe
import qualified Data.Map as Map
import KindLang.Util.Control

----------------------------------------------------------------------------
-- identifiers and related types
----------------------------------------------------------------------------

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
withoutNamespace :: NSID -> String
withoutNamespace (UnqualifiedID s) = s
withoutNamespace (QualifiedID _ s') = withoutNamespace s'

nsidToList :: NSID -> [String]
nsidToList (UnqualifiedID s) = [s]
nsidToList (QualifiedID s s') = s:(nsidToList s')

listToNSID :: [String] -> NSID
listToNSID = foldrn QualifiedID UnqualifiedID
                 
nsidString :: NSID -> String
nsidString = foldl1 (\ a b -> a ++ "::" ++ b) . nsidToList
                 
instance Show NSID where
    show s = "(NSID " ++ show (nsidString s) ++ ")"

----------------------------------------------------------------------------
-- various utility types
----------------------------------------------------------------------------

data PrintableFunction a b = PrintableFunction String (a->b)

instance Show (PrintableFunction a b) where
    show (PrintableFunction name _) = name

instance Eq (PrintableFunction a b) where
    (PrintableFunction n1 _) == (PrintableFunction n2 _) = n1 == n2

instance Ord (PrintableFunction a b) where
    compare (PrintableFunction n1 _) (PrintableFunction n2 _) = compare n1 n2
                                                                
