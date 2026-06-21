module Ast.Syntax
  ( Program (..),
    TopLevel (..),
    Block (..),
    Stmt (..),
    UnaryOp (..),
    StmtKind (..),
    ParsedExpr,
    ParsedProgram,
    ParsedTopLevel,
    ParsedBlock,
    ParsedStmt,
    ParsedRepl,
    ParsedDecl,
    Param (..),
    Repl (..),
    Decl (..),
    Ident (..),
    Mutability (..),
    Sign (..),
    IntSize (..),
    FloatSize (..),
    Type (..),
    BinaryOp (..),
    Expr (..),
    ExprKind (..),
    Value (..),
  )
where

import Data.Map.Strict qualified as Map
import Lexer (AlexPosn)

newtype Program a = Program [TopLevel a]
  deriving (Show, Eq)

type ParsedProgram = Program AlexPosn

newtype TopLevel a = TopLevelStmt (Stmt a)
  deriving (Show, Eq)

type ParsedTopLevel = TopLevel AlexPosn

data Block annot = Block [Stmt annot] (Maybe (Expr annot))
  deriving (Show, Eq)

instance Functor Block where
  fmap f (Block stmts expr) = Block (fmap (fmap f) stmts) (fmap (fmap f) expr)

type ParsedBlock = Block AlexPosn

data Stmt annot = Stmt
  { stmtAnnot :: annot,
    stmtKind :: StmtKind annot
  }
  deriving (Show, Eq)

instance Functor Stmt where
  fmap f (Stmt annot kind) = Stmt (f annot) (fmap f kind)

type ParsedStmt = Stmt AlexPosn

data StmtKind annot
  = DeclStmt (Decl annot)
  | ExprStmt (Expr annot)
  deriving (Show, Eq)

instance Functor StmtKind where
  fmap f = \case
    DeclStmt d -> DeclStmt (fmap f d)
    ExprStmt e -> ExprStmt (fmap f e)

data Repl a
  = ReplStmt (Stmt a)
  | ReplExpr (Expr a)
  deriving (Show, Eq)

type ParsedRepl = Repl AlexPosn

data Decl a
  = ValueDecl Mutability Ident (Maybe Type) (Maybe (Expr a))
  deriving (Show, Eq)

instance Functor Decl where
  fmap f (ValueDecl mutability ident typ expr) =
    ValueDecl mutability ident typ (fmap (fmap f) expr)

type ParsedDecl = Decl AlexPosn

data Param = Param Ident Type
  deriving (Show, Eq)

data Ident = Ident AlexPosn String
  deriving (Show, Eq)

data Mutability = Mutable | Constant
  deriving (Show, Eq)

data Sign = Signed | Unsigned
  deriving (Show, Eq)

data IntSize = I8 | I16 | I32 | I64
  deriving (Show, Eq)

data FloatSize = F32 | F64
  deriving (Show, Eq)

data Type
  = TypeName Ident
  | BoolType AlexPosn
  | IntType AlexPosn Sign IntSize
  | FloatType AlexPosn FloatSize
  | FnType AlexPosn [Type] Type
  | UnitType
  deriving (Show, Eq)

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

data Expr annot = Expr
  { exprAnnot :: annot,
    exprKind :: ExprKind annot
  }
  deriving (Show, Eq)

instance Functor Expr where
  fmap f (Expr annot kind) = Expr (f annot) (fmap f kind)

type ParsedExpr = Expr AlexPosn

data ExprKind annot
  = IntLit Integer
  | FloatLit Double
  | BoolLit Bool
  | BinaryExpr BinaryOp (Expr annot) (Expr annot)
  | UnaryExpr UnaryOp (Expr annot)
  | VarExpr Ident
  | IfExpr (Expr annot) (Block annot) (Maybe (Block annot))
  | FnExpr [Param] Type (Block annot)
  | CallExpr (Expr annot) [Expr annot]
  deriving (Show, Eq)

instance Functor ExprKind where
  fmap f = \case
    IntLit i -> IntLit i
    FloatLit d -> FloatLit d
    BoolLit b -> BoolLit b
    BinaryExpr op l r -> BinaryExpr op (fmap f l) (fmap f r)
    UnaryExpr op e -> UnaryExpr op (fmap f e)
    VarExpr i -> VarExpr i
    IfExpr c t e -> IfExpr (fmap f c) (fmap f t) (fmap (fmap f) e)
    FnExpr params ret body -> FnExpr params ret (fmap f body)
    CallExpr callee args -> CallExpr (fmap f callee) (fmap (fmap f) args)

data Value
  = VInt Sign IntSize Integer
  | VFloat FloatSize Double
  | VBool Bool
  | VUnit
  | VFunction [Param] Type (Block Type) (Map.Map String Value)
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
