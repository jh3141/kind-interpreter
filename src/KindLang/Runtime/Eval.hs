module KindLang.Runtime.Eval where

import KindLang.Data.Value
import KindLang.Data.AST
import KindLang.Data.Error
    
-- fixme: going to need access to program state!
evalAExpr :: AExpr -> KErr Value
evalAExpr (AIntLiteral _ val) = Right $ makeKindInt val
                                
