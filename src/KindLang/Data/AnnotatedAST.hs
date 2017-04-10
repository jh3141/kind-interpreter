{-# LANGUAGE FlexibleContexts #-}
module KindLang.Data.AnnotatedAST where

import Control.Monad.Except
import KindLang.Data.BasicTypes
import KindLang.Data.AST
import KindLang.Data.Error
import KindLang.Locale.ErrorMessages

data ATypeDescriptor =
     ASimpleType NSID | -- fixme: should probably never occur, consider deletion
     AInferableType |  -- fixme: should this be here?
     AFunctionType [ATypeDescriptor] ATypeDescriptor |
     AForAllTypes [String] [TypePredicate] ATypeDescriptor |
     ATypeVariable String |
     ASumType [ATypeDescriptor] |
     ATupleType [ATypeDescriptor] |
     ARecordType NSID [ATypeDescriptor] |
     AReference ATypeDescriptor |
     AResolvedType {
         resolvedTypeRID :: NSID,
         resolvedTypeCanonicalID :: NSID,
         resolvedTypeDefinition :: ADefinition
     }
    deriving (Show, Eq)


data AExpr =
     AIntLiteral ExprAnnotation Int |
     AStringLiteral ExprAnnotation String |
     AVarRef ExprAnnotation NSID |
     AORef ExprAnnotation AExpr NSID |
     -- operators are transformed to function/method applications during
     -- type annotation so do not appear here.
     AFunctionApplication ExprAnnotation AExpr [AExpr] |
     AOMethod ExprAnnotation AExpr ATypeDescriptor NSID [AExpr]
     deriving (Show, Eq)

data ExprAnnotation =
     ExprAnnotation ATypeDescriptor [(String,AnnotationData)]
     deriving (Show, Eq)

data AnnotationData =
     EADOptionTrue |
     EADId NSID
     deriving (Show, Eq)
              
data AStatement =
     AExpression StmtAnnotation AExpr |
     AVarDeclStatement StmtAnnotation String ATypeDescriptor AVariableInitializer |
     AStatementBlock StmtAnnotation [AStatement]
     deriving (Show, Eq)

data StmtAnnotation =
     StmtAnnotation (Maybe ATypeDescriptor) DefList [(String,AnnotationData)]
     deriving (Show, Eq)

data AVariableInitializer =
     VarInitAExpr AExpr |
     VarInitAConstruct [AExpr] |
     VarInitANone
     deriving (Show, Eq)

data AFunctionInstance =
     AFunctionInstance ATypeDescriptor [String] AStatement |
     AInternalFunction ATypeDescriptor InternalFunctionName
     deriving (Show, Eq)
              
data ADefinition =
     AClassDefinition [AClassMember] |
     AFunctionDefinition [AFunctionInstance] |
     AVariableDefinition ATypeDescriptor AVariableInitializer |
     ANamespace (IdentMap ADefinition) |
     AInternalTypeDefinition | -- fixme make this go away
     AInternalObject ATypeDescriptor {- fixme add execution stub -}
     deriving (Show, Eq)

type IdentADefinition = Identified ADefinition
    
data AClassMember = ClassMember String Visibility ADefinition
     deriving (Show, Eq)

type ADefList = [(String,ADefinition)]
    
aexprAnnotation :: AExpr -> ExprAnnotation
aexprAnnotation (AIntLiteral a _) = a
aexprAnnotation (AStringLiteral a _) = a
aexprAnnotation (AVarRef a _) = a
aexprAnnotation (AORef a _ _) = a
aexprAnnotation (AFunctionApplication a _ _) = a
aexprAnnotation (AOMethod a _ _ _ _) = a

exprAnnotationType :: ExprAnnotation -> ATypeDescriptor
exprAnnotationType (ExprAnnotation t _) = t

aexprType :: AExpr -> ATypeDescriptor
aexprType = exprAnnotationType . aexprAnnotation

exprAnnotationCanonicalID :: ExprAnnotation -> Maybe NSID
exprAnnotationCanonicalID (ExprAnnotation _ a) =
    case lookup "CanonicalID" a of
      Just (EADId sid) -> Just sid
      _                -> Nothing

astmtAnnotation :: AStatement -> StmtAnnotation
astmtAnnotation (AExpression a _) = a
astmtAnnotation (AVarDeclStatement a _ _ _) = a
astmtAnnotation (AStatementBlock a _) = a

stmtAnnotationType :: StmtAnnotation -> Maybe ATypeDescriptor
stmtAnnotationType (StmtAnnotation t _ _) = t

astmtType :: AStatement -> Maybe ATypeDescriptor
astmtType = stmtAnnotationType . astmtAnnotation

nullAStatement :: AStatement
nullAStatement = AStatementBlock (StmtAnnotation Nothing [] []) []

stripTypeAnnotations :: ATypeDescriptor -> TypeDescriptor
stripTypeAnnotations (ASimpleType nsid) = SimpleType nsid
stripTypeAnnotations AInferableType = InferableType
stripTypeAnnotations (AFunctionType ats rt) = FunctionType (stripTypeAnnotations <$> ats) (stripTypeAnnotations rt)
stripTypeAnnotations (AForAllTypes strs preds atd) = ForAllTypes strs preds (stripTypeAnnotations atd)
stripTypeAnnotations (ATypeVariable str) = TypeVariable str
stripTypeAnnotations (ASumType ats) = SumType (stripTypeAnnotations <$> ats)
stripTypeAnnotations (ATupleType ats) = TupleType (stripTypeAnnotations <$> ats)
stripTypeAnnotations (ARecordType nsid ats) = RecordType nsid (stripTypeAnnotations <$> ats)
stripTypeAnnotations (AReference at) = Reference (stripTypeAnnotations at)
stripTypeAnnotations (AResolvedType rid cid def) = SimpleType cid

stripFnInstanceAnnotations :: MonadError KindError m => AFunctionInstance -> m FunctionInstance
stripFnInstanceAnnotations (AFunctionInstance td f s) = return (FunctionInstance (stripTypeAnnotations td) f $ stripStatementAnnotations s)
stripFnInstanceAnnotations (AInternalFunction _ n)    = throwError $ InternalError (cannotCreateRawASTForInternalFunction n)

stripStatementAnnotations :: AStatement -> Statement
stripStatementAnnotations (AExpression _ ae) =
    Expression $ stripExprAnnotations ae
stripStatementAnnotations (AVarDeclStatement _ n td vinit) =
    VarDeclStatement n (stripTypeAnnotations td) (stripVarInitAnnotations vinit)
stripStatementAnnotations (AStatementBlock _ astmts) =
    StatementBlock $ map stripStatementAnnotations astmts

stripExprAnnotations :: AExpr -> Expr
stripExprAnnotations (AIntLiteral _ i) = IntLiteral i
stripExprAnnotations (AStringLiteral _ s) = StringLiteral s
stripExprAnnotations (AVarRef _ i) = VarRef i
stripExprAnnotations (AORef _ ae i) = ORef (stripExprAnnotations ae) i
stripExprAnnotations (AFunctionApplication _ ae aes) =
    FunctionApplication (stripExprAnnotations ae) $ map stripExprAnnotations aes
stripExprAnnotations (AOMethod _ ae _ i aes) =
    OMethod (stripExprAnnotations ae) i $ map stripExprAnnotations aes

stripVarInitAnnotations :: AVariableInitializer -> VariableInitializer
stripVarInitAnnotations (VarInitAExpr ae) = VarInitExpr $ stripExprAnnotations ae
stripVarInitAnnotations (VarInitAConstruct aes) =
    VarInitConstruct $ map stripExprAnnotations aes


afnInstanceBody :: AFunctionInstance -> AStatement
afnInstanceBody (AFunctionInstance _ _ b) = b
afnInstanceBody (AInternalFunction _ _) = nullAStatement -- fixme shouldn't this have a correct result type?
                                          
afnInstanceType :: AFunctionInstance -> ATypeDescriptor
afnInstanceType (AFunctionInstance td _ _) = td
afnInstanceType (AInternalFunction td _) = td

afnInstanceArgs :: AFunctionInstance -> [String]
afnInstanceArgs (AFunctionInstance _ a _) = a
afnInstanceArgs (AInternalFunction td _) = map (const "") (functionTypeArgs $ stripTypeAnnotations td)
