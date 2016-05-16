module KindLang.Locale.ErrorMessages where

incompatibleReturn :: String
incompatibleReturn =
    "Type returned by function body incompatible with declared type"

noReturn :: String
noReturn =
    "Function body should return value"

filterRequiresIdentifier :: String
filterRequiresIdentifier =
    "Filtered import must specify both module and identifier (perhaps you " ++
    "wanted 'module::*'?)"

requiredCanonicalID :: String
requiredCanonicalID = "expression annotation did not contain a canonical id"

insertedIntoNonNamespace :: Show a => a -> String
insertedIntoNonNamespace  rid =
    "attempted to insert item into non-namespace definition of catalogue " ++
    (show rid)
