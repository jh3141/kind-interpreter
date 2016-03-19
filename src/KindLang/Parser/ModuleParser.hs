module KindLang.Parser.ModuleParser where

import Text.Parsec
-- import Control.Applicative ((<*), (*>))
import Control.Monad
import Data.Maybe

import KindLang.Data.AST
import KindLang.Util.Control
import KindLang.Parser.Combinators

-- common separator characters
semicolon :: Parsec String u Char
semicolon = char ';'
colon :: Parsec String u Char
colon = char ':'
comma :: Parsec String u Char
comma = char ','
            
_module_ :: Parsec String u Module
_module_ = 
    do
        header <- optionMaybe $ withws _moduleHeader_
        body <- manyTill (withws (_import_ </> _declaration_)) eof
        case foldr updateLists ([],[]) body of
            (imports, declarations) -> return $ Module header imports declarations    
    where
        updateLists (Left a)  (listA, listB) = (a:listA, listB)
        updateLists (Right b) (listA, listB) = (listA, b:listB)

_identifier_ :: Parsec String u String
_identifier_ = startChar >>= \x -> many continueChar >>= \y -> return (x:y)
    where
        startChar = letter <|> oneOf "_~"
        continueChar = startChar <|> digit

_scopeOp_ :: Parsec String u String
_scopeOp_ = string "::"
            
_scopedID_ :: Parsec String u ScopedID
_scopedID_ = liftM (foldrn QualifiedID UnqualifiedID) $
                   sepBy1Lazy (withtws _identifier_) (withtws _scopeOp_)

_moduleHeader_ :: Parsec String u ScopedID
_moduleHeader_ = string "module" >> withws _scopedID_ <* semicolon

-- FIXME handle more forms of import!
_import_ :: Parsec String u ModuleImport
_import_ = do
    string "import" 
    x <- withws _scopedID_
    wildcard <- optionMaybe $ string "::" >> withws (char '*')
    withtws semicolon
    return $ UnqualifiedModuleImport x (isJust wildcard)


type DeclarationP u = Parsec String u (String,Definition)
_declaration_ :: DeclarationP u
_declaration_ =
    _classDeclaration_ <|>
    (try _variableDeclaration_) <|>
    _functionDeclaration_
    --fixme eliminating the try here would speed things up a bit

_classDeclaration_ :: DeclarationP u
_classDeclaration_ = do
    withtws $ string "class"
    ident <- withtws _identifier_
    body <- braced
            (many $ fmap (declarationToClassMember Public)
                         (withtws _declaration_))
    return (ident, ClassDefinition body)

           
declarationToClassMember :: Visibility -> (String,Definition) -> ClassMember
declarationToClassMember v (n,d) = ClassMember n v d
                                   
_variableDeclaration_ :: DeclarationP u
_variableDeclaration_ = do
    ident <- withtws _identifier_
    withtws colon
    typeName <- withtws _typeDescriptor_
    initopt <- optionMaybe $ withtws (_functionApplication_ </> _initExpr_)
    
    semicolon
    return (ident, VariableDefinition typeName
                     (makeVarInit initopt))
    where
      makeVarInit :: Maybe (Either [Expr] Expr) -> VariableInitializer
      makeVarInit Nothing             = VarInitNone
      makeVarInit (Just (Left args))  = VarInitConstruct args
      makeVarInit (Just (Right expr)) = VarInitExpr expr

_typeDescriptor_ :: Parsec String u TypeDescriptor
_typeDescriptor_ = fmap SimpleType _scopedID_  -- or type expression

_functionApplication_ :: Parsec String u [Expr]                      
_functionApplication_ = bracketed (withtws _expr_ `sepBy` withtws comma)
                        
_expr_ :: Parsec String u Expr
_expr_ = fmap VarRef  _identifier_ -- TODO!

_initExpr_ :: Parsec String u Expr
_initExpr_ = char '=' >> withlws _expr_

_functionDeclaration_ :: DeclarationP u
_functionDeclaration_ =
    withtws _identifier_ <&>
    liftM3 FunctionDefinition
           (withtws $ bracketed
               (withtws _parameterDeclaration_ `sepBy` withtws comma))
           (maybeOrInferable <$>
               optionMaybe (withtws colon >> withtws _typeDescriptor_))
           (braced $ withtws _expr_ `sepBy` withtws semicolon)

_parameterDeclaration_ :: Parsec String u (String,TypeDescriptor)
_parameterDeclaration_ =
    withtws _identifier_ <&>
    fmap maybeOrInferable
         (optionMaybe (withtws colon >> withtws _typeDescriptor_))
         
maybeOrInferable :: Maybe TypeDescriptor -> TypeDescriptor
maybeOrInferable = maybe InferableType id
