module KindLang.Data.AST where

import Data.Maybe
import KindLang.Data.BasicTypes

type DefList = [(String,Definition)]
    
data ModuleImport = 
     UnqualifiedModuleImport NSID Bool |
     QualifiedModuleImport NSID Bool (Maybe NSID)
     deriving (Show, Eq)
     
data Module = Module {
     moduleName :: Maybe NSID,
     moduleImportList :: [ModuleImport],
     moduleDeclarationList :: DefList     -- FIXME shouldn't this be called moduleDefinitionList?
} deriving (Show, Eq)

type InternalFunctionName = String
    
data FunctionInstance =
     FunctionInstance TypeDescriptor [String] Statement |
     AFunctionInstance TypeDescriptor [String] AStatement |
     InternalFunction TypeDescriptor InternalFunctionName
     deriving (Show, Eq)
              
data Definition =
     ClassDefinition [ClassMember] |
     FunctionDefinition [FunctionInstance] |
     VariableDefinition TypeDescriptor VariableInitializer |
     Namespace (IdentMap Definition) |
     InternalTypeDefinition | -- fixme make this go away
     InternalObject TypeDescriptor {- fixme add execution stub -}
     deriving (Show, Eq)
type IdentDefinition = Identified Definition
    
data ClassMember = ClassMember String Visibility Definition
     deriving (Show, Eq)

data Visibility = Public | Protected | Private
     deriving (Show, Eq)
              
data TypeDescriptor =
     SimpleType NSID |
     InferableType |
     ResolvedType {
         resolvedTypeRID :: NSID,
         resolvedTypeCanonicalID :: NSID,
         resolvedTypeDefinition :: Definition
     } |
     FunctionType [TypeDescriptor] TypeDescriptor |
     ForAllTypes [String] [TypePredicate] TypeDescriptor |
     TypeVariable String |
     SumType [TypeDescriptor] |
     TupleType [TypeDescriptor] |
     RecordType NSID [TypeDescriptor] |
     Reference TypeDescriptor
     deriving (Show, Eq)

data TypePredicate =
     NotType TypePredicate |
     TypesEqual TypeDescriptor TypeDescriptor
     deriving (Show, Eq)
                   
data Expr =
     Annotated AExpr |
     IntLiteral Int |
     StringLiteral String |
     VarRef NSID |
     ORef Expr NSID |
     BinOp String Expr Expr |
     PrefixOp String Expr |
     FunctionApplication Expr [Expr] |
     OMethod Expr NSID [Expr]
     deriving (Show, Eq)

data AExpr =
     AIntLiteral ExprAnnotation Int |
     AStringLiteral ExprAnnotation String |
     AVarRef ExprAnnotation NSID |
     AORef ExprAnnotation AExpr NSID |
     -- operators are transformed to function/method applications during
     -- type annotation so do not appear here.
     AFunctionApplication ExprAnnotation AExpr [AExpr] |
     AOMethod ExprAnnotation AExpr TypeDescriptor NSID [AExpr] |
     -- internal references generated during resolution, e.g. internal functions
     AInternalRef ExprAnnotation NSID
     deriving (Show, Eq)

data ExprAnnotation =
     ExprAnnotation TypeDescriptor [(String,AnnotationData)]
     deriving (Show, Eq)

data AnnotationData =
     EADOptionTrue |
     EADId NSID
     deriving (Show, Eq)
              
data Statement =
     Expression Expr |
     VarDeclStatement String TypeDescriptor VariableInitializer |
     StatementBlock [Statement]
     deriving (Show, Eq)

data AStatement =
     AExpression StmtAnnotation AExpr |
     AVarDeclStatement StmtAnnotation String TypeDescriptor VariableInitializer |
     AStatementBlock StmtAnnotation [AStatement]
     deriving (Show, Eq)

data StmtAnnotation =
     StmtAnnotation (Maybe TypeDescriptor) DefList [(String,AnnotationData)]
     deriving (Show, Eq)
              
data VariableInitializer =
     VarInitNone |
     VarInitExpr Expr |
     VarInitAExpr AExpr |
     VarInitConstruct [Expr] |
     VarInitAConstruct [AExpr]
     deriving (Show, Eq)


-- functions for common manipulations of the AST
maybeOrInferable :: Maybe TypeDescriptor -> TypeDescriptor
maybeOrInferable = fromMaybe InferableType

fnDefInstances :: Definition -> [FunctionInstance]
fnDefInstances (FunctionDefinition i) = i
fnDefInstances _ = []
                   
                                 
definitionTypeName :: Definition -> String
definitionTypeName (ClassDefinition _) = "class"
definitionTypeName (FunctionDefinition _) = "function"
definitionTypeName (VariableDefinition _ _) = "variable"
definitionTypeName (Namespace _) = "namespace"
definitionTypeName (InternalTypeDefinition) = "internal-type"
definitionTypeName (InternalObject _) = "internal-object"
                                      
aexprAnnotation :: AExpr -> ExprAnnotation
aexprAnnotation (AIntLiteral a _) = a
aexprAnnotation (AStringLiteral a _) = a
aexprAnnotation (AVarRef a _) = a
aexprAnnotation (AORef a _ _) = a                      
aexprAnnotation (AFunctionApplication a _ _) = a
aexprAnnotation (AOMethod a _ _ _ _) = a
aexprAnnotation (AInternalRef a _) = a
                                     
exprAnnotationType :: ExprAnnotation -> TypeDescriptor
exprAnnotationType (ExprAnnotation t _) = t
                                                
aexprType :: AExpr -> TypeDescriptor
aexprType = exprAnnotationType . aexprAnnotation

exprAnnotationCanonicalID :: ExprAnnotation -> Maybe NSID
exprAnnotationCanonicalID (ExprAnnotation _ a) =
    case lookup "CanonicalID" a of
      Just (EADId sid) -> Just sid
      _                -> Nothing
                         
classMemberName :: ClassMember -> String
classMemberName (ClassMember name _ _) = name

namespaceCatalogue :: Definition -> IdentMap Definition
namespaceCatalogue (Namespace cat) = cat
namespaceCatalogue t = error ("not a namespace: " ++ definitionTypeName t)

-- | Convert a statement list to a single statement. Empty lists become
-- a statement block, as do lists with multiple statements, but lists with
-- just one statement are unwrapped.
statementListToStatement :: [Statement] -> Statement
statementListToStatement (s:[]) = s
statementListToStatement ss = StatementBlock ss

astmtAnnotation :: AStatement -> StmtAnnotation
astmtAnnotation (AExpression a _) = a
astmtAnnotation (AVarDeclStatement a _ _ _) = a
astmtAnnotation (AStatementBlock a _) = a

stmtAnnotationType :: StmtAnnotation -> Maybe TypeDescriptor
stmtAnnotationType (StmtAnnotation t _ _) = t

astmtType :: AStatement -> Maybe TypeDescriptor
astmtType = stmtAnnotationType . astmtAnnotation

nullStatement :: Statement
nullStatement = StatementBlock []
nullAStatement :: AStatement
nullAStatement = AStatementBlock (StmtAnnotation Nothing [] []) []
                 
fnInstanceBody :: FunctionInstance -> Either Statement AStatement
fnInstanceBody (FunctionInstance _ _ b) = Left b
fnInstanceBody (AFunctionInstance _ _ ab) = Right ab
fnInstanceBody (InternalFunction _ _) = Right $ nullAStatement
                                        
fnInstanceType :: FunctionInstance -> TypeDescriptor
fnInstanceType (FunctionInstance td _ _) = td
fnInstanceType (AFunctionInstance td _ _) = td
fnInstanceType (InternalFunction td _) = td
                                         
fnInstanceArgs :: FunctionInstance -> [String]
fnInstanceArgs (FunctionInstance _ a _) = a
fnInstanceArgs (AFunctionInstance _ a _) = a
fnInstanceArgs (InternalFunction td _) = map (const "") (functionTypeArgs td)

functionTypeReturn :: TypeDescriptor -> TypeDescriptor
functionTypeReturn (FunctionType _ r) = r
functionTypeReturn n = n -- maybe should be an error?

functionTypeArgs :: TypeDescriptor -> [TypeDescriptor]
functionTypeArgs (FunctionType r _) = r
functionTypeArgs _ = []


                    
