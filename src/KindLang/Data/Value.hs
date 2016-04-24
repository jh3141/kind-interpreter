module KindLang.Data.Value where

-- fixme probably want a lower-level implementation of this, so we can manage memory
-- ourselves
data Value = KindInt Int


getKindInt :: Value -> Int
getKindInt (KindInt a) = a
                            
makeKindInt :: Int -> Value
makeKindInt a = (KindInt a)
                
