module KindLang.Locale.ErrorMessages where

incompatibleReturn :: String
incompatibleReturn =
    "Type returned by function body incompatible with declared type"

noReturn :: String
noReturn =
    "Function body should return value"

filterRequiresIdentifier :: String
filterRequiresIdentifier =
    "Filtered import must specify both module and identifier (perhaps you wanted 'module::*'?)"
