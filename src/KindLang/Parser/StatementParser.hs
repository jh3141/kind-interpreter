module KindLang.Parser.StatementParser where

import Text.Parsec
    
import KindLang.Data.AST
import KindLang.Parser.State
import KindLang.Parser.Combinators
import KindLang.Parser.BasicTokens
import KindLang.Parser.ExpressionParser (expr_)

type StatementP = Parser Statement

stmt_ :: StatementP
stmt_ =
    -- a variable declaration needs lookahead before it can be
    -- identified, so use 'try' around it, and put it first (as
    -- the expression it could be confused with would be slower
    -- to parse)
    withtws $ try $ varDeclStmt_ <* semicolon <|>
    (Expression <$> withtws expr_ <* semicolon)

varDeclStmt_ :: StatementP
varDeclStmt_ = do
    ident <- withtws identifier_
    withtws colon
    typeDescriptor <- maybeOrInferable <$>
                      (optionMaybe $ withtws typeDescriptor_)
    varInit <- variableInitializer_
    
    case (typeDescriptor,varInit) of
      (InferableType,VarInitConstruct _) ->
          fail "Cannot infer variable type with constructor call"
      (InferableType,VarInitNone) ->
          fail "Cannot infer variable type without initialization"
      _ ->
          return $ VarDeclStatement ident typeDescriptor varInit
           
variableInitializer_ :: Parser VariableInitializer
variableInitializer_ =
    makeVarInit <$> (optionMaybe
                     $ withtws (functionApplication_ </> initExpr_))    
    where
      makeVarInit :: Maybe (Either [Expr] Expr) -> VariableInitializer
      makeVarInit Nothing             = VarInitNone
      makeVarInit (Just (Left args))  = VarInitConstruct args
      makeVarInit (Just (Right expr)) = VarInitExpr expr

functionApplication_ :: Parser [Expr]                      
functionApplication_ =
    breakCommas <$> bracketed (withtws expr_)
    where
      breakCommas (BinOp "," a b) = a:(breakCommas b)
      breakCommas a               = [a]
                        
initExpr_ :: Parser Expr
initExpr_ = char '=' >> withlws expr_
                      
