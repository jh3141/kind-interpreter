module KindLang.Parser.StatementParser where

import Text.Parsec
    
import KindLang.Data.AST
import KindLang.Parser.State
import KindLang.Parser.Combinators
import KindLang.Parser.BasicTokens
import KindLang.Parser.ExpressionParser (expr_)

type StatementP = Parser Statement

stmt_ :: StatementP
stmt_ = do
    -- a variable declaration needs lookahead before it can be
    -- identified, so check for the start of a variable declaration
    maybeIdent <- tryOpenVarDeclStmt_
    case maybeIdent of
       Just ident -> withtws (varDeclStmt_ ident) <* semicolon
       Nothing    -> simpleStatement_

simpleStatement_ :: StatementP
simpleStatement_ =
    (StatementBlock <$> (withtws (braced (many (withtws stmt_))))) <|>
    (Expression <$> withtws expr_ <* semicolon)
                       
-- attempt to parse the beginning of a variable declaration (up to the,
-- whitespace following the colon) and return the identifier if successful
-- or Nothing if unsuccessful:
tryOpenVarDeclStmt_ :: Parser (Maybe String)
tryOpenVarDeclStmt_ =
    optionMaybe (try (withtws identifier_ <* withtws colon))

-- given the identifier returned from tryOpenVarDeclStmt_, parse the
-- remainder of the statement
varDeclStmt_ :: String -> StatementP
varDeclStmt_ ident = do
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
      breakCommas (BinOp "," a b) = a:breakCommas b
      breakCommas a               = [a]
                        
initExpr_ :: Parser Expr
initExpr_ = char '=' >> withlws expr_
                      
