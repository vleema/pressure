module Ast.Syntax
  ( Program (..),
    TopLevel (..),
    Block (..),
    ParsedExpr (..),
    ParsedExprKind (..),
    ParsedStmt (..),
    ParsedStmtKind (..),
    ParsedDecl (..),
    ParsedAssign (..),
    TypedAssign (..),
    ParsedRepl,
    ParsedProgram,
    ParsedTopLevel,
    ParsedBlock,
    TypedExpr (..),
    TypedExprKind (..),
    TypedStmt (..),
    TypedStmtKind (..),
    TypedDecl (..),
    TypedRepl,
    TypedProgram,
    TypedTopLevel,
    TypedBlock,
    Param (..),
    TypedParam (..),
    Repl (..),
    Ident (..),
    Mutability (..),
    Sign (..),
    IntSize (..),
    FloatSize (..),
    TypeSyntax (..),
    TypeSyntaxKind (..),
    Type (..),
    BinaryOp (..),
    UnaryOp (..),
    Value (..),
    identPos,
    identName,
    prettyType,
    prettyBinaryOp,
    prettyUnaryOp,
  )
where

import Data.List (intercalate)
import Lexer (AlexPosn (..))

newtype Program stmt = Program [TopLevel stmt]
  deriving (Show, Eq)

newtype TopLevel stmt = TopLevelStmt stmt
  deriving (Show, Eq)

data Block stmt expr = Block [stmt] (Maybe expr)
  deriving (Show, Eq)

data Repl stmt expr
  = ReplStmt stmt
  | ReplExpr expr
  deriving (Show, Eq)

type ParsedProgram = Program ParsedStmt

type ParsedTopLevel = TopLevel ParsedStmt

type ParsedBlock = Block ParsedStmt ParsedExpr

type ParsedRepl = Repl ParsedStmt ParsedExpr

type TypedProgram = Program TypedStmt

type TypedTopLevel = TopLevel TypedStmt

type TypedBlock = Block TypedStmt TypedExpr

type TypedRepl = Repl TypedStmt TypedExpr

data ParsedStmt = ParsedStmt
  { parsedStmtPos :: AlexPosn,
    parsedStmtKind :: ParsedStmtKind
  }
  deriving (Show, Eq)

data ParsedStmtKind
  = ParsedDeclStmt ParsedDecl
  | ParsedExprStmt ParsedExpr
  | ParsedAssignStmt ParsedAssign
  deriving (Show, Eq)

data TypedStmt = TypedStmt
  { typedStmtPos :: AlexPosn,
    typedStmtKind :: TypedStmtKind
  }
  deriving (Show, Eq)

data TypedStmtKind
  = TypedDeclStmt TypedDecl
  | TypedExprStmt TypedExpr
  | TypedAssignStmt TypedAssign
  deriving (Show, Eq)

data ParsedDecl
  = ParsedValueDecl Mutability Ident (Maybe TypeSyntax) (Maybe ParsedExpr)
  deriving (Show, Eq)

data TypedDecl
  = TypedValueDecl Mutability Ident Type (Maybe TypedExpr)
  deriving (Show, Eq)

data ParsedAssign
  = ParsedAssign Ident ParsedExpr
  deriving (Show, Eq)

data TypedAssign
  = TypedAssign String TypedExpr
  deriving (Show, Eq)

data Param = Param Ident TypeSyntax
  deriving (Show, Eq)

data TypedParam = TypedParam Ident Type
  deriving (Show, Eq)

data Ident = Ident AlexPosn String
  deriving (Show, Eq)

identPos :: Ident -> AlexPosn
identPos (Ident pos _) = pos

identName :: Ident -> String
identName (Ident _ name) = name

data Mutability = Mutable | Constant
  deriving (Show, Eq)

data Sign = Signed | Unsigned
  deriving (Show, Eq)

data IntSize = I8 | I16 | I32 | I64
  deriving (Show, Eq)

data FloatSize = F32 | F64
  deriving (Show, Eq)

data TypeSyntax = TypeSyntax
  { typePos :: AlexPosn,
    typeKind :: TypeSyntaxKind
  }
  deriving (Show, Eq)

data TypeSyntaxKind
  = NameSyntax String
  | BoolSyntax
  | IntSyntax Sign IntSize
  | FloatSyntax FloatSize
  | FnSyntax [TypeSyntax] TypeSyntax
  | UnitSyntax
  deriving (Show, Eq)

data Type
  = BoolT
  | IntT Sign IntSize
  | FloatT FloatSize
  | FnT [Type] Type
  | UnitT
  deriving (Show, Eq)

prettyType :: Type -> String
prettyType = \case
  BoolT -> "bool"
  IntT Signed I8 -> "i8"
  IntT Signed I16 -> "i16"
  IntT Signed I32 -> "i32"
  IntT Signed I64 -> "i64"
  IntT Unsigned I8 -> "u8"
  IntT Unsigned I16 -> "u16"
  IntT Unsigned I32 -> "u32"
  IntT Unsigned I64 -> "u64"
  FloatT F32 -> "f32"
  FloatT F64 -> "f64"
  FnT params ret -> "fn(" ++ intercalate ", " (map prettyType params) ++ ") -> " ++ prettyType ret
  UnitT -> "()"

data UnaryOp
  = NegOp
  | NotOp
  | AmpersandOp
  deriving (Show, Eq)

data BinaryOp
  = AddOp
  | SubOp
  | MulOp
  | DivOp
  | AndOp
  | OrOp
  | EqOp
  | NeqOp
  | LtOp
  | LeqOp
  | GtOp
  | GeqOp
  deriving (Show, Eq)

data ParsedExpr = ParsedExpr
  { parsedExprPos :: AlexPosn,
    parsedExprKind :: ParsedExprKind
  }
  deriving (Show, Eq)

data ParsedExprKind
  = ParsedIntLit Integer
  | ParsedFloatLit Double
  | ParsedBoolLit Bool
  | ParsedBinaryExpr BinaryOp ParsedExpr ParsedExpr
  | ParsedUnaryExpr UnaryOp ParsedExpr
  | ParsedVarExpr Ident
  | ParsedIfExpr ParsedExpr ParsedBlock (Maybe ParsedBlock)
  | ParsedFnExpr [Param] TypeSyntax ParsedBlock
  | ParsedCallExpr ParsedExpr [ParsedExpr]
  deriving (Show, Eq)

data TypedExpr = TypedExpr
  { typedExprPos :: AlexPosn,
    typedExprType :: Type,
    typedExprKind :: TypedExprKind
  }
  deriving (Show, Eq)

data TypedExprKind
  = TypedIntLit Integer
  | TypedFloatLit Double
  | TypedBoolLit Bool
  | TypedBinaryExpr BinaryOp TypedExpr TypedExpr
  | TypedUnaryExpr UnaryOp TypedExpr
  | TypedVarExpr Ident
  | TypedIfExpr TypedExpr TypedBlock (Maybe TypedBlock)
  | TypedFnExpr [TypedParam] Type TypedBlock
  | TypedCallExpr TypedExpr [TypedExpr]
  deriving (Show, Eq)

data Value
  = VInt Sign IntSize Integer
  | VFloat FloatSize Double
  | VBool Bool
  | VUnit
  | VFunction [TypedParam] Type TypedBlock
  | VEmpty
  deriving (Eq)

instance Show Value where
  show = \case
    VInt _ _ i -> show i
    VFloat _ f -> show f
    VBool True -> "true"
    VBool False -> "false"
    VUnit -> "()"
    VFunction {} -> "<function>"
    VEmpty -> undefined

prettyBinaryOp :: BinaryOp -> String
prettyBinaryOp = \case
  AddOp -> "+"
  SubOp -> "-"
  MulOp -> "*"
  DivOp -> "/"
  AndOp -> "and"
  OrOp -> "or"
  EqOp -> "=="
  NeqOp -> "!="
  LtOp -> "<"
  LeqOp -> "<="
  GtOp -> ">"
  GeqOp -> ">="

prettyUnaryOp :: UnaryOp -> String
prettyUnaryOp = \case
  NegOp -> "-"
  NotOp -> "!"
  AmpersandOp -> "&"
