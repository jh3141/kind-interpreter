module KindLang.Data.AST where

import KindLang.Data.BasicTypes
import KindLang.Data.MStat
import KindLang.Data.Types
import KindLang.Util.Control

data ASTNodeInfo = ASTNodeInfo {
      nodeId :: Integer
    } deriving (Show, Eq)
                 
type DefList = [(String,Definition)]

data ModuleImport = 
     UnqualifiedModuleImport ASTNodeInfo NSID Bool |
     QualifiedModuleImport ASTNodeInfo NSID Bool (Maybe NSID)
     deriving (Show, Eq)
     
data Module = Module {
     moduleName :: Maybe NSID,
     moduleImportList :: [ModuleImport],
     moduleDeclarationList :: DefList     -- FIXME shouldn't this be called moduleDefinitionList?
} deriving (Show, Eq)

type InternalFunctionName = String
    
data FunctionInstance =
     FunctionInstance ASTNodeInfo TypeDescriptor [String] Statement |
     InternalFunction ASTNodeInfo TypeDescriptor InternalFunctionName
     deriving (Show, Eq)
              
data Definition =
     ClassDefinition ASTNodeInfo [ClassMember] |
     FunctionDefinition ASTNodeInfo [FunctionInstance] |
     VariableDefinition ASTNodeInfo TypeDescriptor VariableInitializer |
     Namespace ASTNodeInfo (IdentMap Definition) |
     InternalTypeDefinition | -- fixme make this go away
     InternalObject TypeDescriptor {- fixme add execution stub -}
     deriving (Show, Eq)
              
type IdentDefinition = Identified Definition
    
data ClassMember = ClassMember String Visibility Definition
     deriving (Show, Eq)

data Expr =
     IntLiteral ASTNodeInfo Int |
     StringLiteral ASTNodeInfo String |
     VarRef ASTNodeInfo NSID |
     ORef ASTNodeInfo Expr NSID |
     BinOp ASTNodeInfo String Expr Expr |
     PrefixOp ASTNodeInfo String Expr |
     FunctionApplication ASTNodeInfo Expr [Expr] |
     OMethod ASTNodeInfo Expr NSID [Expr]
     deriving (Show, Eq)

data Statement =
     Expression ASTNodeInfo Expr |
     VarDeclStatement ASTNodeInfo String TypeDescriptor VariableInitializer |
     StatementBlock ASTNodeInfo [Statement]
     deriving (Show, Eq)

data VariableInitializer =
     VarInitNone |
     VarInitExpr Expr |
     VarInitConstruct [Expr]
     deriving (Show, Eq)


-- functions for common manipulations of the AST
fnDefInstances :: Definition -> [FunctionInstance]
fnDefInstances (FunctionDefinition _ i) = i
fnDefInstances _ = []

definitionTypeName :: Definition -> String
definitionTypeName (ClassDefinition _ _) = "class"
definitionTypeName (FunctionDefinition _ _) = "function"
definitionTypeName (VariableDefinition _ _ _) = "variable"
definitionTypeName (Namespace _ _) = "namespace"
definitionTypeName (InternalTypeDefinition) = "internal-type"
definitionTypeName (InternalObject _) = "internal-object"

classMemberName :: ClassMember -> String
classMemberName (ClassMember name _ _) = name

namespaceCatalogue :: Definition -> IdentMap Definition
namespaceCatalogue (Namespace _ cat) = cat
namespaceCatalogue t = error ("not a namespace: " ++ definitionTypeName t)

-- | Initialize an AST node, automatically generating an appropriate ASTNodeInfo
-- structure for it.  If the specified node constructor requires arguments, they
-- may be supplied using the '$#' operator defined in "KindLang.Util.Control".
newNode :: MStat m s => (ASTNodeInfo -> c) -> m s c
newNode constructor = (constructor . ASTNodeInfo) <$> kstatUniqueId
                       
-- | Convert a statement list to a single statement. Empty lists become
-- a statement block, as do lists with multiple statements, but lists with
-- just one statement are unwrapped.
statementListToStatement :: MStat m s => [Statement] -> m s Statement
statementListToStatement (s:[]) = return s
statementListToStatement ss = newNode StatementBlock $# ss

nullStatement :: MStat m s => m s Statement
nullStatement = newNode StatementBlock $# []

fnInstanceBody :: FunctionInstance -> Statement
fnInstanceBody (FunctionInstance _ _ _ b) = b
fnInstanceBody (InternalFunction _ _ _) = error "Internal functions do not have bodies"
                                          
fnInstanceType :: FunctionInstance -> TypeDescriptor
fnInstanceType (FunctionInstance _ td _ _) = td
fnInstanceType (InternalFunction _ td _) = td

fnInstanceArgs :: FunctionInstance -> [String]
fnInstanceArgs (FunctionInstance _ _ a _) = a
fnInstanceArgs (InternalFunction _ td _) = map (const "") (functionTypeArgs td)

fnInstCompatible :: FunctionInstance -> [TypeDescriptor] -> Bool
fnInstCompatible = fnTypeCompatible . fnInstanceType


-- this function operations on type descriptors, but is defined here rather than Types.hs in order to
-- prevent circular dependency issues.
                                         
functionTypeReturn :: MStat m s => TypeDescriptor -> m s TypeDescriptor
functionTypeReturn (FunctionType _ r) = return r
functionTypeReturn t                  = internalError $ "Expected function type, got " ++ show t

definitionToType :: Definition -> TypeDescriptor
definitionToType (ClassDefinition _ members) = undefined -- FIXME metaclass!
definitionToType (FunctionDefinition _ (inst:[])) = fnInstanceType inst
definitionToType (FunctionDefinition _ _) = undefined    -- FIXME overloading!
definitionToType (VariableDefinition _ td _) = td
definitionToType (Namespace _ _) = error "Namespaces do not have types"
definitionToType (InternalObject td) = td

