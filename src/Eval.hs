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
    Mutability (..),
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
import Ast.Typecheck (TypedBlock, TypedExpr, TypedProgram, TypedStmt)
import Control.Monad.Except (Except, MonadError (throwError))
import Control.Monad.State (StateT, get, lift, modify, put)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)

-- Types

data Error
  = RuntimeError String
  | BreakSignal Value
  | ContinueSignal
  | ReturnSignal Value
  deriving (Eq, Show)

type Env = [Map String Value]

type Eval a = StateT Env (Except Error) a

data RuntimeNumber
  = RuntimeInt Sign IntSize Integer
  | RuntimeFloat FloatSize Double

-- Environment helpers

lookupName :: String -> Env -> Maybe Value
lookupName _ [] = Nothing
lookupName name (scope : rest) =
  case Map.lookup name scope of
    Just v -> Just v
    Nothing -> lookupName name rest

bindInCurrentScope :: String -> Value -> Env -> Env
bindInCurrentScope name val [] = [Map.singleton name val]
bindInCurrentScope name val (scope : rest) =
  Map.insert name val scope : rest

globalEnv :: Env -> Env
globalEnv [] = []
globalEnv [scope] = [scope]
globalEnv (_ : rest) = globalEnv rest

pushScope :: Env -> Env
pushScope env = Map.empty : env

popScope :: Env -> Env
popScope [] = []
popScope (_ : rest) = rest

withScope :: Eval a -> Eval a
withScope action = do
  modify pushScope
  result <- action
  modify popScope
  return result

-- Values

asNumber :: Value -> Maybe RuntimeNumber
asNumber = \case
  VInt s k i -> Just (RuntimeInt s k i)
  VFloat k f -> Just (RuntimeFloat k f)
  _ -> Nothing

defaultValue :: Type -> Value
defaultValue = \case
  IntType _ s k -> VInt s k 0
  FloatType _ k -> VFloat k 0
  BoolType _ -> VBool False
  FnType _ _ _ -> VEmpty
  UnitType -> VUnit
  TypeName _ -> VEmpty

liftMaybe :: Error -> Maybe a -> Eval a
liftMaybe err = maybe (lift $ throwError err) pure

withNumbers :: (RuntimeNumber -> RuntimeNumber -> Eval Value) -> Value -> Value -> Eval Value
withNumbers f va vb =
  case (asNumber va, asNumber vb) of
    (Just na, Just nb) -> f na nb
    _ -> throwError $ RuntimeError "invalid operands"

-- Expressions

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
evalIfExpr c t mElse = do
  v <- evalExpr c
  case v of
    VBool True -> withScope (evalBlock t)
    VBool False -> maybe (return VUnit) (withScope . evalBlock) mElse
    _ -> throwError $ RuntimeError "if condition must be bool"

evalVarExpr :: String -> Eval Value
evalVarExpr name = do
  env <- get
  case lookupName name env of
    Just v -> return v
    Nothing -> throwError $ RuntimeError ("undefined variable: " ++ name)

evalFnExpr :: [Param] -> Type -> TypedBlock -> Eval Value
evalFnExpr params ret body =
  return $ VFunction params ret body

evalCallExpr :: TypedExpr -> [TypedExpr] -> Eval Value
evalCallExpr callee args = do
  fn <- evalExpr callee
  argVals <- mapM evalExpr args
  callValue fn argVals

callValue :: Value -> [Value] -> Eval Value
callValue (VFunction params _ body) argVals = do
  if length params /= length argVals
    then throwError $ RuntimeError "wrong number of arguments"
    else do
      callerEnv <- get
      modify $ const $ bindArgs params argVals (pushScope (globalEnv callerEnv))
      val <- evalBlock body
      modify $ const callerEnv
      return val
callValue _ _ = throwError $ RuntimeError "attempted to call non-function"

bindArgs :: [Param] -> [Value] -> Env -> Env
bindArgs params argVals env = foldl bind env (zip params argVals)
  where
    bind e (Param (Ident _ name) _, val) = bindInCurrentScope name val e

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
evalNumericBin intOp floatOp va vb = withNumbers go va vb
  where
    go (RuntimeInt s k a) (RuntimeInt _ _ b) = return (VInt s k (intOp a b))
    go (RuntimeFloat k a) (RuntimeFloat _ b) = return (VFloat k (floatOp a b))
    go _ _ = throwError $ RuntimeError "internal error"

evalDiv :: Value -> Value -> Eval Value
evalDiv va vb = case vb of
  VInt _ _ 0 -> throwError $ RuntimeError "division by zero"
  VFloat _ 0 -> throwError $ RuntimeError "division by zero"
  _ -> evalNumericBin div (/) va vb

evalNumericCmp :: (Integer -> Integer -> Bool) -> (Double -> Double -> Bool) -> Value -> Value -> Eval Value
evalNumericCmp intCmp floatCmp va vb = withNumbers go va vb
  where
    go (RuntimeInt _ _ a) (RuntimeInt _ _ b) = return (VBool (intCmp a b))
    go (RuntimeFloat _ a) (RuntimeFloat _ b) = return (VBool (floatCmp a b))
    go _ _ = throwError $ RuntimeError "internal error"

evalEq :: Value -> Value -> Eval Value
evalEq (VBool a) (VBool b) = return (VBool (a == b))
evalEq va vb = withNumbers go va vb
  where
    go (RuntimeInt _ _ a) (RuntimeInt _ _ b) = return (VBool (a == b))
    go (RuntimeFloat _ a) (RuntimeFloat _ b) = return (VBool (a == b))
    go _ _ = throwError $ RuntimeError "internal error"

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

-- Statements

evalStmt :: TypedStmt -> Eval Value
evalStmt = \case
  s | isFunctionItemStmt s -> return VUnit
  Stmt _ (DeclStmt (ValueDecl _ (Ident _ name) mType mExpr)) -> evalDeclExpr name mType mExpr
  Stmt _ (ExprStmt expr) -> evalExpr expr >> return VUnit

evalDeclExpr :: String -> Maybe Type -> Maybe TypedExpr -> Eval Value
evalDeclExpr name mType mExpr = do
  val <- case mExpr of
    Just e -> evalExpr e
    Nothing -> case mType of
      Just t -> return (defaultValue t)
      Nothing -> throwError $ RuntimeError "declaration lacks both type and initializer"
  modify (bindInCurrentScope name val)
  return VUnit

-- Function items

isFunctionItemStmt :: TypedStmt -> Bool
isFunctionItemStmt (Stmt _ (DeclStmt (ValueDecl Constant _ _ (Just (Expr _ (FnExpr {})))))) = True
isFunctionItemStmt _ = False

installFunctionItems :: [TypedStmt] -> Eval ()
installFunctionItems stmts = do
  let fns = mapMaybe functionItem stmts
  env <- get
  let extendedEnv = foldl addFn env fns
        where
          addFn env' (name, params, ret, body) =
            let closure = VFunction params ret body
             in bindInCurrentScope name closure env'
  put extendedEnv

functionItem :: TypedStmt -> Maybe (String, [Param], Type, TypedBlock)
functionItem = \case
  (Stmt _ (DeclStmt (ValueDecl Constant (Ident _ name) _ (Just (Expr _ (FnExpr params ret body)))))) -> Just (name, params, ret, body)
  _ -> Nothing

-- Blocks

evalBlock :: TypedBlock -> Eval Value
evalBlock (Block stmts expr) = do
  installFunctionItems stmts
  mapM_ evalStmt stmts
  maybe (return VUnit) evalExpr expr

-- Programs

evalProgram :: TypedProgram -> Eval Value
evalProgram (Program toplevels) = do
  modify pushScope
  let stmts = map topLevelStmt toplevels
  installFunctionItems stmts
  mapM_ evalStmt stmts
  return VUnit
  where
    topLevelStmt (TopLevelStmt stmt) = stmt

-- REPL

evalReplInput :: Repl Type -> Eval Value
evalReplInput = \case
  ReplExpr e -> evalExpr e
  ReplStmt s | isFunctionItemStmt s -> installFunctionItems [s] >> return VUnit
  ReplStmt s -> evalStmt s >> return VUnit
