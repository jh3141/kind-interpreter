module KindLang.Data.Value where

-- fixme probably want a lower-level implementation of this, so we can manage memory
-- ourselves
data Value = KindInt Int


kindGetInt :: Value -> Int
kindGetInt (KindInt a) = a
                         
              
