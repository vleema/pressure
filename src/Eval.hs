module Eval
  ( Error (..),
    RuntimeError (..),
    Eval,
    Env,
    evalExpr,
    evalStmt,
    evalBlock,
    evalRepl,
    evalReplInput,
    evalProgram,
    errorInfo,
  )
where

import Ast.Syntax
  ( BinaryOp (..),
    Block (..),
    FloatSize (..),
    Ident (..),
    IntSize (..),
    Mutability (..),
    Program (..),
    Repl (..),
    ReplInput (ReplExpr, ReplStmt),
    Sign (..),
    TopLevel (..),
    Type (..),
    TypedAssign (..),
    TypedDecl (..),
    TypedExpr (..),
    TypedExprKind (..),
    TypedParam (..),
    TypedReplInput,
    TypedStmt (..),
    TypedStmtKind (..),
    UnaryOp (..),
    Value (..),
  )
import Ast.Typecheck (TypedBlock, TypedProgram, TypedRepl)
import Control.Monad.Except (Except, MonadError (catchError, throwError))
import Control.Monad.State (StateT, get, modify, put)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Lexer (AlexPosn (..), prettyPosn)

-- Types

data Error
  = RuntimeError RuntimeError
  | BreakSignal Value
  | ContinueSignal
  | ReturnSignal Value
  deriving (Eq, Show)

data RuntimeError
  = DivisionByZero AlexPosn
  | Overflow AlexPosn
  | Underflow AlexPosn
  deriving (Eq, Show)

panic :: String -> Eval a
panic = error . ("panic: " ++)

panicAt :: AlexPosn -> String -> Eval a
panicAt pos msg = error $ prettyPosn pos ++ ": panic: " ++ msg

errorInfo :: Error -> (Maybe AlexPosn, String)
errorInfo = \case
  RuntimeError (DivisionByZero pos) -> (Just pos, "division by zero")
  RuntimeError (Overflow pos) -> (Just pos, "integer overflow")
  RuntimeError (Underflow pos) -> (Just pos, "integer underflow")
  _ -> (Nothing, "internal error: unexpected control flow")

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

updateInScope :: String -> Value -> Env -> Env
updateInScope _ _ [] = []
updateInScope name val (scope : rest)
  | Map.member name scope = Map.insert name val scope : rest
  | otherwise = scope : updateInScope name val rest

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
  IntT s k -> VInt s k 0
  FloatT k -> VFloat k 0
  BoolT -> VBool False
  FnT _ _ -> VEmpty
  UnitT -> VUnit

withNumbers :: AlexPosn -> (RuntimeNumber -> RuntimeNumber -> Eval Value) -> Value -> Value -> Eval Value
withNumbers pos f va vb =
  case (asNumber va, asNumber vb) of
    (Just na, Just nb) -> f na nb
    _ -> panicAt pos "invalid operands reached evaluator"

-- Expressions

evalExpr :: TypedExpr -> Eval Value
evalExpr (TypedExpr pos _ kind) = case kind of
  TypedIntLit i -> return (VInt Signed I32 i)
  TypedFloatLit f -> return (VFloat F64 f)
  TypedBoolLit b -> return (VBool b)
  TypedUnitLit -> return VUnit
  TypedUnaryExpr op e -> evalUnaryExpr pos op e
  TypedBinaryExpr op l r -> evalBinaryExpr pos op l r
  TypedVarExpr (Ident identPos name) -> evalVarExpr identPos name
  TypedIfExpr c t elseBlock -> evalIfExpr pos c t elseBlock
  TypedWhileExpr c body mElse -> evalWhileExpr pos c body mElse
  TypedFnExpr params ret body -> evalFnExpr params ret body
  TypedCallExpr callee args -> evalCallExpr pos callee args
  TypedBreakExpr mExpr -> evalBreakExpr pos mExpr
  TypedContinueExpr -> evalContinueExpr pos

evalIfExpr :: AlexPosn -> TypedExpr -> TypedBlock -> Maybe TypedBlock -> Eval Value
evalIfExpr pos c t mElse = do
  v <- evalExpr c
  case v of
    VBool True -> withScope (evalBlock t)
    VBool False -> maybe (return VUnit) (withScope . evalBlock) mElse
    _ -> panicAt pos "if condition must be bool reached evaluator"

evalWhileExpr :: AlexPosn -> TypedExpr -> TypedBlock -> Maybe TypedBlock -> Eval Value
evalWhileExpr pos cond body mElse = loop
  where
    loop = do
      v <- evalExpr cond
      case v of
        VBool True -> do
          result <- tryLoopIteration (withScope (evalBlock body))
          maybe loop return result
        VBool False ->
          maybe (return VUnit) (withScope . evalBlock) mElse
        _ -> panicAt pos "while condition must be bool reached evaluator"

tryLoopIteration :: Eval Value -> Eval (Maybe Value)
tryLoopIteration action =
  catchError
    (action >> return Nothing)
    ( \case
        BreakSignal val -> return (Just val)
        ContinueSignal -> return Nothing
        other -> throwError other
    )

evalBreakExpr :: AlexPosn -> TypedExpr -> Eval Value
evalBreakExpr _ e = evalExpr e >>= \val -> throwError (BreakSignal val)

evalContinueExpr :: AlexPosn -> Eval Value
evalContinueExpr _ = throwError ContinueSignal

evalVarExpr :: AlexPosn -> String -> Eval Value
evalVarExpr pos name = do
  env <- get
  case lookupName name env of
    Just v -> return v
    Nothing -> panicAt pos ("undefined variable '" ++ name ++ "' reached evaluator")

evalFnExpr :: [TypedParam] -> Type -> TypedBlock -> Eval Value
evalFnExpr params ret body =
  return $ VFunction params ret body

evalCallExpr :: AlexPosn -> TypedExpr -> [TypedExpr] -> Eval Value
evalCallExpr pos callee args = do
  fn <- evalExpr callee
  argVals <- mapM evalExpr args
  callValue pos fn argVals

callValue :: AlexPosn -> Value -> [Value] -> Eval Value
callValue pos (VFunction params _ body) argVals = do
  if length params /= length argVals
    then panicAt pos ("wrong number of arguments: expected " ++ show (length params) ++ ", got " ++ show (length argVals))
    else do
      callerEnv <- get
      modify $ const $ bindArgs params argVals (pushScope (globalEnv callerEnv))
      val <- evalBlock body
      modify $ const callerEnv
      return val
callValue pos _ _ = panicAt pos "attempted to call non-function reached evaluator"

bindArgs :: [TypedParam] -> [Value] -> Env -> Env
bindArgs params argVals env = foldl bind env (zip params argVals)
  where
    bind e (TypedParam (Ident _ name) _, val) = bindInCurrentScope name val e

evalUnaryExpr :: AlexPosn -> UnaryOp -> TypedExpr -> Eval Value
evalUnaryExpr pos op e = do
  ve <- evalExpr e
  case op of
    NegOp -> evalNumericUn negate negate ve
    NotOp -> evalBooleanUn not ve
    AmpersandOp -> panicAt pos "not implemented: unary '&'"

evalNumericUn :: (Integer -> Integer) -> (Double -> Double) -> Value -> Eval Value
evalNumericUn intOp floatOp v =
  case asNumber v of
    Just (RuntimeInt s k i) -> return (VInt s k (intOp i))
    Just (RuntimeFloat k d) -> return (VFloat k (floatOp d))
    Nothing -> panic "expected number"

evalBooleanUn :: (Bool -> Bool) -> Value -> Eval Value
evalBooleanUn op = \case
  VBool b -> return (VBool $ op b)
  _ -> panic "expected bool"

evalBinaryExpr :: AlexPosn -> BinaryOp -> TypedExpr -> TypedExpr -> Eval Value
evalBinaryExpr pos op l r = do
  vl <- evalExpr l
  vr <- evalExpr r
  case op of
    AddOp -> evalNumericBin pos (+) (+) vl vr
    SubOp -> evalNumericBin pos (-) (-) vl vr
    MulOp -> evalNumericBin pos (*) (*) vl vr
    DivOp -> evalDiv pos vl vr
    AndOp -> evalBoolBin pos (&&) vl vr
    OrOp -> evalBoolBin pos (||) vl vr
    EqOp -> evalEq pos vl vr
    NeqOp -> evalNeq pos vl vr
    LtOp -> evalNumericCmp pos (<) (<) vl vr
    LeqOp -> evalNumericCmp pos (<=) (<=) vl vr
    GtOp -> evalNumericCmp pos (>) (>) vl vr
    GeqOp -> evalNumericCmp pos (>=) (>=) vl vr

evalNumericBin :: AlexPosn -> (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> Value -> Value -> Eval Value
evalNumericBin pos intOp floatOp = withNumbers pos go
  where
    go (RuntimeInt s k a) (RuntimeInt _ _ b) = return (VInt s k (intOp a b))
    go (RuntimeFloat k a) (RuntimeFloat _ b) = return (VFloat k (floatOp a b))
    go _ _ = panic "type mismatch in numeric bin"

evalDiv :: AlexPosn -> Value -> Value -> Eval Value
evalDiv pos va vb = case vb of
  VInt _ _ 0 -> throwError $ RuntimeError $ DivisionByZero pos
  VFloat _ 0 -> throwError $ RuntimeError $ DivisionByZero pos
  _ -> evalNumericBin pos div (/) va vb

evalNumericCmp :: AlexPosn -> (Integer -> Integer -> Bool) -> (Double -> Double -> Bool) -> Value -> Value -> Eval Value
evalNumericCmp pos intCmp floatCmp = withNumbers pos go
  where
    go (RuntimeInt _ _ a) (RuntimeInt _ _ b) = return (VBool (intCmp a b))
    go (RuntimeFloat _ a) (RuntimeFloat _ b) = return (VBool (floatCmp a b))
    go _ _ = panic "type mismatch in numeric cmp"

evalEq :: AlexPosn -> Value -> Value -> Eval Value
evalEq _ (VBool a) (VBool b) = return (VBool (a == b))
evalEq pos va vb = withNumbers pos go va vb
  where
    go (RuntimeInt _ _ a) (RuntimeInt _ _ b) = return (VBool (a == b))
    go (RuntimeFloat _ a) (RuntimeFloat _ b) = return (VBool (a == b))
    go _ _ = panic "type mismatch in eq"

evalNeq :: AlexPosn -> Value -> Value -> Eval Value
evalNeq pos va vb = do
  v <- evalEq pos va vb
  case v of
    VBool b -> return (VBool (not b))
    _ -> panic "type mismatch in neq"

evalBoolBin :: AlexPosn -> (Bool -> Bool -> Bool) -> Value -> Value -> Eval Value
evalBoolBin _ op va vb = case (va, vb) of
  (VBool a, VBool b) -> return (VBool (op a b))
  _ -> panic "invalid operands reached evaluator"

-- Statements

evalStmt :: TypedStmt -> Eval Value
evalStmt = \case
  s | isFunctionItemStmt s -> return VUnit
  TypedStmt _ (TypedDeclStmt (TypedValueDecl _ (Ident pos name) typ mExpr)) -> evalDeclExpr pos name typ mExpr
  TypedStmt _ (TypedAssignStmt (TypedAssign name expr)) -> do
    val <- evalExpr expr
    modify (updateInScope name val)
    return VUnit
  TypedStmt _ (TypedExprStmt expr) -> evalExpr expr >> return VUnit

evalDeclExpr :: AlexPosn -> String -> Type -> Maybe TypedExpr -> Eval Value
evalDeclExpr _ name typ mExpr = do
  val <- case mExpr of
    Just e -> evalExpr e
    Nothing -> return (defaultValue typ)
  modify (bindInCurrentScope name val)
  return VUnit

-- Function items

isFunctionItemStmt :: TypedStmt -> Bool
isFunctionItemStmt (TypedStmt _ (TypedDeclStmt (TypedValueDecl Constant _ _ (Just (TypedExpr _ _ (TypedFnExpr {})))))) = True
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

functionItem :: TypedStmt -> Maybe (String, [TypedParam], Type, TypedBlock)
functionItem = \case
  TypedStmt _ (TypedDeclStmt (TypedValueDecl Constant (Ident _ name) _ (Just (TypedExpr _ _ (TypedFnExpr params ret body))))) -> Just (name, params, ret, body)
  _ -> Nothing

-- TODO: Grrrr, remove this duplication
functionStmt :: TypedStmt -> Maybe TypedStmt
functionStmt s = case s of
  TypedStmt _ (TypedDeclStmt (TypedValueDecl Constant (Ident _ _) _ (Just (TypedExpr _ _ (TypedFnExpr _ _ _))))) -> Just s
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

evalRepl :: TypedRepl -> Eval Value
evalRepl (Repl inputs) = do
  modify pushScope
  installFunctionItems $ mapMaybe isStmtAndFunctionItem inputs
  mapM_ evalReplInput inputs
  return VUnit
  where
    isStmtAndFunctionItem (ReplStmt s) = functionStmt s
    isStmtAndFunctionItem _ = Nothing

evalReplInput :: TypedReplInput -> Eval Value
evalReplInput = \case
  ReplExpr e -> evalExpr e
  ReplStmt s | isFunctionItemStmt s -> installFunctionItems [s] >> return VUnit
  ReplStmt s -> evalStmt s >> return VUnit
