{-# LANGUAGE RankNTypes #-}
module KindLang.Parser.ModuleParser where

import Text.Parsec
import Control.Monad
import Control.Monad.Trans
import Control.Applicative ((<**>))
import Data.Maybe
import Data.Either

import KindLang.Data.AST
import KindLang.Data.BasicTypes
import KindLang.Data.Types
import KindLang.Parser.Combinators
import KindLang.Parser.BasicTokens
import KindLang.Parser.State
import KindLang.Parser.StatementParser
import KindLang.Util.Control
    
module_ :: Parser Module
module_ = 
    do
        header <- optionMaybe $ withws moduleHeader_
        body <- manyTill (withws (import_ </> declaration_)) eof
        return ((uncurry $ Module header) (partitionEithers body))

moduleHeader_ :: Parser NSID
moduleHeader_ = string "module" >> withws scopedID_ <* semicolon

-- FIXME handle more forms of import!
import_ :: Parser ModuleImport
import_ = do
    string "import" 
    x <- withws scopedID_
    wildcard <- optionMaybe $ string "::" >> withws (char '*')
    withtws semicolon
    newNodeP UnqualifiedModuleImport $# x $# (isJust wildcard)


type DeclarationP = Parser (String,Definition)
declaration_ :: DeclarationP
declaration_ =
    classDeclaration_ <|>
    try variableDeclaration_ <|>
    functionDeclaration_
    --fixme eliminating the try here would speed things up a bit

classDeclaration_ :: DeclarationP
classDeclaration_ = do
    withtws $ string "class"
    ident <- withtws identifier_
    body <- braced
            (many $ declarationToClassMember Public <$>
                         (withtws declaration_))
    definition <- newNodeP ClassDefinition $# body
    return (ident, definition)

           
declarationToClassMember :: Visibility -> (String,Definition) -> ClassMember
declarationToClassMember v (n,d) = ClassMember n v d
                                   
variableDeclaration_ :: DeclarationP
variableDeclaration_ = do
    ident <- withtws identifier_
    withtws colon
    typeName <- withtws typeDescriptor_
    initopt <- variableInitializer_
    definition <- newNodeP VariableDefinition $# typeName $# initopt
    semicolon
    return (ident, definition)


functionDeclaration_ :: DeclarationP
functionDeclaration_ =
    withtws identifier_ <&>
            (newNodeP FunctionDefinition <*>
             (withtws functionInstance_ `sepBy1` withtws comma))
    
functionInstance_ :: Parser FunctionInstance            
functionInstance_ =
    newNodeP makeFunctionInstance <*>
           (withtws $ bracketed
                    (withtws parameterDeclaration_ `sepBy` withtws comma)) <*>
           (maybeOrInferable <$> optionMaybe
                                 (withtws colon >> withtws typeDescriptor_)) <*>
           ((braced $ many $ withtws stmt_) >>= (lift . statementListToStatement))
    where
      makeFunctionInstance :: ASTNodeInfo -> [(String,TypeDescriptor)] -> TypeDescriptor -> 
                              Statement -> FunctionInstance
      makeFunctionInstance info params retType body =
          FunctionInstance info (FunctionType (snd <$> params) retType)
                           (fst <$> params)
                           body
           
parameterDeclaration_ :: Parser (String,TypeDescriptor)
parameterDeclaration_ =
    withtws identifier_ <&>
    fmap maybeOrInferable
         (optionMaybe (withtws colon >> withtws typeDescriptor_))
         
