module Eval
  ( Error (..),
    Eval,
    Env,
    evalExpr,
    evalStmt,
    evalBlock,
    evalReplInput,
    evalProgram,
  )
where

import Ast.Syntax
  ( BinaryOp (..),
    Block (..),
    Decl (..),
    Expr (..),
    ExprKind (..),
    FloatSize (..),
    Ident (..),
    IntSize (..),
    Param (..),
    Program (..),
    Repl (..),
    Sign (..),
    Stmt (..),
    StmtKind (..),
    TopLevel (..),
    Type (..),
    UnaryOp (..),
    Value (..),
  )
import Ast.Typecheck (TypedBlock, TypedExpr, TypedProgram, TypedStmt, TypedTopLevel)
import Control.Monad.Except (Except, MonadError (throwError))
import Control.Monad.State (StateT, get, lift, modify)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

data Error
  = RuntimeError String
  | BreakSignal Value
  | ContinueSignal
  | ReturnSignal Value
  deriving (Eq, Show)

type Env = Map String Value

type Eval a = StateT Env (Except Error) a

evalExpr :: TypedExpr -> Eval Value
evalExpr (Expr _ kind) = case kind of
  IntLit i -> return (VInt Signed I32 i)
  FloatLit f -> return (VFloat F64 f)
  BoolLit b -> return (VBool b)
  UnaryExpr op e -> evalUnaryExpr op e
  BinaryExpr op l r -> evalBinaryExpr op l r
  VarExpr (Ident _ i) -> evalVarExpr i
  IfExpr c t elseBlock -> evalIfExpr c t elseBlock
  FnExpr params ret body -> evalFnExpr params ret body
  CallExpr callee args -> evalCallExpr callee args

evalIfExpr :: TypedExpr -> TypedBlock -> Maybe TypedBlock -> Eval Value
evalIfExpr c t me = do
  v <- evalExpr c
  case v of
    VBool True -> evalScopedBlock t
    VBool False -> maybe (return VUnit) evalScopedBlock me
    _ -> throwError $ RuntimeError "if condition must be bool"

evalStmt :: TypedStmt -> Eval Value
evalStmt (Stmt _ stmt) = case stmt of
  DeclStmt (ValueDecl _ (Ident _ i) mt me) -> evalDeclExpr i mt me
  ExprStmt expr -> evalExpr expr >> return VUnit

evalBlock :: TypedBlock -> Eval Value
evalBlock (Block stmts expr) = do
  mapM_ evalStmt stmts
  maybe (return VUnit) evalExpr expr

evalScopedBlock :: TypedBlock -> Eval Value
evalScopedBlock block = do
  env <- get
  val <- evalBlock block
  modify (const env)
  return val

evalFnExpr :: [Param] -> Type -> TypedBlock -> Eval Value
evalFnExpr params ret body = do
  env <- get
  return $ VFunction params ret body env

evalCallExpr :: TypedExpr -> [TypedExpr] -> Eval Value
evalCallExpr callee args = do
  fn <- evalExpr callee
  argVals <- mapM evalExpr args
  callValue fn argVals

callValue :: Value -> [Value] -> Eval Value
callValue (VFunction params _ body capturedEnv) argVals = do
  if length params /= length argVals
    then throwError $ RuntimeError "wrong number of arguments"
    else do
      callerEnv <- get
      modify $ const $ bindArgs params argVals capturedEnv
      val <- evalBlock body
      modify $ const callerEnv
      return val
callValue _ _ = throwError $ RuntimeError "attempted to call non-function"

bindArgs :: [Param] -> [Value] -> Env -> Env
bindArgs params argVals env = foldr bind env (zip params argVals)
  where
    bind (Param (Ident _ name) _, val) = Map.insert name val

evalUnaryExpr :: UnaryOp -> TypedExpr -> Eval Value
evalUnaryExpr op e = do
  ve <- evalExpr e
  case op of
    NegOp -> evalNumericUn negate negate ve
    NotOp -> evalBooleanUn not ve
    AmpersandOp -> throwError $ RuntimeError "not implemented"

evalNumericUn :: (Integer -> Integer) -> (Double -> Double) -> Value -> Eval Value
evalNumericUn intOp floatOp v = do
  n <- liftMaybe (RuntimeError "internal error") (asNumber v)
  case n of
    RuntimeInt s k i -> return (VInt s k (intOp i))
    RuntimeFloat k d -> return (VFloat k (floatOp d))

evalBooleanUn :: (Bool -> Bool) -> Value -> Eval Value
evalBooleanUn op = \case
  VBool b -> return (VBool $ op b)
  _ -> throwError $ RuntimeError "internal error"

evalBinaryExpr :: BinaryOp -> TypedExpr -> TypedExpr -> Eval Value
evalBinaryExpr op l r = do
  vl <- evalExpr l
  vr <- evalExpr r
  case op of
    AddOp -> evalNumericBin (+) (+) vl vr
    SubOp -> evalNumericBin (-) (-) vl vr
    MulOp -> evalNumericBin (*) (*) vl vr
    DivOp -> evalDiv vl vr
    AndOp -> evalBoolBin (&&) vl vr
    OrOp -> evalBoolBin (||) vl vr
    EqOp -> evalEq vl vr
    NeqOp -> evalNeq vl vr
    LtOp -> evalNumericCmp (<) (<) vl vr
    LeqOp -> evalNumericCmp (<=) (<=) vl vr
    GtOp -> evalNumericCmp (>) (>) vl vr
    GeqOp -> evalNumericCmp (>=) (>=) vl vr

evalNumericBin :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> Value -> Value -> Eval Value
evalNumericBin intOp floatOp va vb =
  case (asNumber va, asNumber vb) of
    (Just na, Just nb) -> case (na, nb) of
      (RuntimeInt s k a, RuntimeInt _ _ b) -> return (VInt s k (intOp a b))
      (RuntimeFloat k a, RuntimeFloat _ b) -> return (VFloat k (floatOp a b))
      _ -> throwError $ RuntimeError "internal error"
    _ -> throwError $ RuntimeError "invalid operands"

evalDiv :: Value -> Value -> Eval Value
evalDiv va vb = case vb of
  VInt _ _ 0 -> throwError $ RuntimeError "division by zero"
  VFloat _ 0 -> throwError $ RuntimeError "division by zero"
  _ -> evalNumericBin div (/) va vb

evalNumericCmp :: (Integer -> Integer -> Bool) -> (Double -> Double -> Bool) -> Value -> Value -> Eval Value
evalNumericCmp intCmp floatCmp va vb =
  case (asNumber va, asNumber vb) of
    (Just na, Just nb) -> case (na, nb) of
      (RuntimeInt _ _ a, RuntimeInt _ _ b) -> return (VBool (intCmp a b))
      (RuntimeFloat _ a, RuntimeFloat _ b) -> return (VBool (floatCmp a b))
      _ -> throwError $ RuntimeError "internal error"
    _ -> throwError $ RuntimeError "invalid operands"

evalEq :: Value -> Value -> Eval Value
evalEq (VBool a) (VBool b) = return (VBool (a == b))
evalEq va vb =
  case (asNumber va, asNumber vb) of
    (Just na, Just nb) -> case (na, nb) of
      (RuntimeInt _ _ a, RuntimeInt _ _ b) -> return (VBool (a == b))
      (RuntimeFloat _ a, RuntimeFloat _ b) -> return (VBool (a == b))
      _ -> throwError $ RuntimeError "internal error"
    _ -> throwError $ RuntimeError "invalid operands"

evalNeq :: Value -> Value -> Eval Value
evalNeq va vb = do
  v <- evalEq va vb
  case v of
    VBool b -> return (VBool (not b))
    _ -> throwError $ RuntimeError "internal error"

evalBoolBin :: (Bool -> Bool -> Bool) -> Value -> Value -> Eval Value
evalBoolBin op va vb = case (va, vb) of
  (VBool a, VBool b) -> return (VBool (op a b))
  _ -> throwError $ RuntimeError "invalid operands"

evalVarExpr :: String -> Eval Value
evalVarExpr n = do
  env <- get
  case Map.lookup n env of
    Just v -> return v
    Nothing -> throwError $ RuntimeError ("undefined variable: " ++ n)

evalDeclExpr :: String -> Maybe Type -> Maybe TypedExpr -> Eval Value
evalDeclExpr n mt me = do
  val <- case me of
    Just e -> evalExpr e
    Nothing -> case mt of
      Just t -> return (defaultValue t)
      Nothing -> throwError $ RuntimeError "declaration lacks both type and initializer"
  modify (Map.insert n val)
  return VUnit

evalReplInput :: Repl Type -> Eval Value
evalReplInput = \case
  ReplExpr e -> evalExpr e
  ReplStmt s -> evalStmt s >> return VUnit

evalProgram :: TypedProgram -> Eval Value
evalProgram (Program stmts) = mapM_ evalTopLevel stmts >> return VUnit

evalTopLevel :: TypedTopLevel -> Eval Value
evalTopLevel (TopLevelStmt stmt) = evalStmt stmt

defaultValue :: Type -> Value
defaultValue = \case
  IntType _ s k -> VInt s k 0
  FloatType _ k -> VFloat k 0
  BoolType _ -> VBool False
  FnType _ _ _ -> VEmpty
  UnitType -> VUnit
  TypeName _ -> VEmpty

data RuntimeNumber
  = RuntimeInt Sign IntSize Integer
  | RuntimeFloat FloatSize Double

asNumber :: Value -> Maybe RuntimeNumber
asNumber = \case
  VInt s k i -> Just (RuntimeInt s k i)
  VFloat k f -> Just (RuntimeFloat k f)
  _ -> Nothing

liftMaybe :: Error -> Maybe a -> Eval a
liftMaybe err = maybe (lift $ throwError err) pure
