module KindLang.Parser.ModuleParser where

import Text.Parsec
import Control.Monad
import Data.Maybe
import Data.Either

import KindLang.Data.AST
import KindLang.Parser.Combinators
import KindLang.Parser.BasicTokens
import KindLang.Parser.State
import KindLang.Parser.ExpressionParser

module_ :: Parser Module
module_ = 
    do
        header <- optionMaybe $ withws moduleHeader_
        body <- manyTill (withws (import_ </> declaration_)) eof
        return ((uncurry $ Module header) (partitionEithers body))

moduleHeader_ :: Parser ScopedID
moduleHeader_ = string "module" >> withws scopedID_ <* semicolon

-- FIXME handle more forms of import!
import_ :: Parser ModuleImport
import_ = do
    string "import" 
    x <- withws scopedID_
    wildcard <- optionMaybe $ string "::" >> withws (char '*')
    withtws semicolon
    return $ UnqualifiedModuleImport x (isJust wildcard)


type DeclarationP = Parser (String,Definition)
declaration_ :: DeclarationP
declaration_ =
    classDeclaration_ <|>
    (try variableDeclaration_) <|>
    functionDeclaration_
    --fixme eliminating the try here would speed things up a bit

classDeclaration_ :: DeclarationP
classDeclaration_ = do
    withtws $ string "class"
    ident <- withtws identifier_
    body <- braced
            (many $ fmap (declarationToClassMember Public)
                         (withtws declaration_))
    return (ident, ClassDefinition body)

           
declarationToClassMember :: Visibility -> (String,Definition) -> ClassMember
declarationToClassMember v (n,d) = ClassMember n v d
                                   
variableDeclaration_ :: DeclarationP
variableDeclaration_ = do
    ident <- withtws identifier_
    withtws colon
    typeName <- withtws typeDescriptor_
    initopt <- optionMaybe $ withtws (functionApplication_ </> initExpr_)
    
    semicolon
    return (ident, VariableDefinition typeName
                     (makeVarInit initopt))
    where
      makeVarInit :: Maybe (Either [Expr] Expr) -> VariableInitializer
      makeVarInit Nothing             = VarInitNone
      makeVarInit (Just (Left args))  = VarInitConstruct args
      makeVarInit (Just (Right expr)) = VarInitExpr expr

typeDescriptor_ :: Parser TypeDescriptor
typeDescriptor_ = fmap SimpleType scopedID_  -- or type expression

functionApplication_ :: Parser [Expr]                      
functionApplication_ = bracketed (withtws expr_ `sepBy` withtws comma)
                        
initExpr_ :: Parser Expr
initExpr_ = char '=' >> withlws expr_

functionDeclaration_ :: DeclarationP
functionDeclaration_ =
    withtws identifier_ <&>
    liftM3 FunctionDefinition
           (withtws $ bracketed
               (withtws parameterDeclaration_ `sepBy` withtws comma))
           (maybeOrInferable <$>
               optionMaybe (withtws colon >> withtws typeDescriptor_))
           (braced $ many $ withtws expr_  <* withtws semicolon)

parameterDeclaration_ :: Parser (String,TypeDescriptor)
parameterDeclaration_ =
    withtws identifier_ <&>
    fmap maybeOrInferable
         (optionMaybe (withtws colon >> withtws typeDescriptor_))
         
maybeOrInferable :: Maybe TypeDescriptor -> TypeDescriptor
maybeOrInferable = maybe InferableType id
