module Pressure.Builtins where

import Control.Monad (unless, zipWithM_)
import Control.Monad.Except (MonadError (throwError), liftEither)
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict qualified as Map
import Pressure.Interpreter.Env (Eval)
import Pressure.Interpreter.Error (EvalError (RuntimeError), RuntimeError (..), panicAt)
import Pressure.Interpreter.Value (Value (..), ValueEnv)
import Pressure.Language.Ast (Ident (..), TypedExpr (..), TypedExprKind (..))
import Pressure.Language.Lexer (AlexPosn)
import Pressure.Language.Types
import Pressure.Typechecker.Env (Check, TypeEnv)
import Pressure.Typechecker.Error (Error (..))
import Text.Read (readMaybe)

initialValueEnv :: ValueEnv
initialValueEnv =
  [ Map.fromList
      [ ("@read", VBuiltin "@read"),
        ("@printf", VBuiltin "@printf"),
        ("@as", VBuiltin "@as"),
        ("@push", VBuiltin "@push"),
        ("@pop", VBuiltin "@pop")
      ]
  ]

initialTypeEnv :: TypeEnv
initialTypeEnv =
  [ Map.fromList
      [ ("@read", (FnT [] StringT, Constant)),
        ("@printf", (FnT [StringT] UnitT, Constant)),
        ("@as", (FnT [TypeT, AnyTypeT] AnyTypeT, Constant)),
        ("@push", (FnT [ArrT AnyTypeT, AnyTypeT] (ArrT AnyTypeT), Constant)),
        ("@pop",  (FnT [ArrT AnyTypeT] (ArrT AnyTypeT), Constant))
      ]
  ]

dispatchBuiltin :: AlexPosn -> String -> [Value] -> Eval Value
dispatchBuiltin pos name args = case name of
  "@read" -> dispatchRead pos args
  "@printf" -> dispatchPrintf pos args
  "@as" -> dispatchAs pos args
  "@push" -> dispatchPush pos args
  "@pop" -> dispatchPop pos args
  _ -> panicAt pos ("unknown builtin: " ++ name)

dispatchRead :: AlexPosn -> [Value] -> Eval Value
dispatchRead pos = \case
  [] -> VString <$> liftIO getLine
  _ -> panicAt pos "@read takes no arguments"

dispatchPrintf :: AlexPosn -> [Value] -> Eval Value
dispatchPrintf pos = \case
  VString fmt : args -> do
    let placeholders = countPlaceholders fmt
    if placeholders /= length args
      then panicAt pos ("@printf: expected " ++ show placeholders ++ " arguments for placeholders, got " ++ show (length args))
      else do
        let rendered = renderFormat fmt args
        liftIO $ putStr rendered
        return VUnit
  _ -> panicAt pos "@printf requires a string format as first argument"

checkPrintfCall :: AlexPosn -> TypedExpr -> [TypedExpr] -> Check TypedExpr
checkPrintfCall pos callee args = case args of
  [] -> liftEither $ Left $ InvalidPrintf pos "expected at least a format string argument"
  (fmtExpr : formatArgs) -> do
    unless (typedExprType fmtExpr == StringT) $ liftEither $ Left $ InvalidPrintf pos "first argument must be a string"
    case typedExprKind fmtExpr of
      TypedStringLit fmt -> do
        let placeholders = countPlaceholders fmt
        unless (placeholders == length formatArgs) $ liftEither $ Left $ InvalidPrintf pos $ placeholdersErr placeholders
        zipWithM_ checkPrintableFormatArg [1 ..] formatArgs
      _ -> liftEither $ Left $ InvalidPrintf pos "format string must be a literal"
    return $ TypedExpr pos UnitT (TypedCallExpr callee args)
    where
      placeholdersErr placeholders = "expected " ++ show placeholders ++ " arguments for placeholders, got " ++ show (length formatArgs)

checkPrintableFormatArg :: Int -> TypedExpr -> Check ()
checkPrintableFormatArg idx arg =
  unless (isPrintable (typedExprType arg)) $
    liftEither $
      Left $
        InvalidPrintf (typedExprPos arg) $
          "argument " ++ show idx ++ " has non-printable type '" ++ prettyType (typedExprType arg) ++ "'"

isPrintable :: Type -> Bool
isPrintable = \case
  IntT {} -> True
  FloatT {} -> True
  BoolT -> True
  StringT -> True
  UnitT -> True
  ArrT {} -> True
  _ -> False

isPrintf :: TypedExpr -> Bool
isPrintf (TypedExpr _ _ (TypedVarExpr (Ident _ "@printf"))) = True
isPrintf _ = False

countPlaceholders :: String -> Int
countPlaceholders = go 0
  where
    go n [] = n
    go n ('{' : '}' : rest) = go (n + 1) rest
    go n (_ : rest) = go n rest

renderFormat :: String -> [Value] -> String
renderFormat fmt args = go fmt args ""
  where
    go [] _ acc = acc
    go ('{' : '}' : rest) (v : vs) acc = go rest vs (acc ++ formatValue v)
    go (c : rest) args' acc = go rest args' (acc ++ [c])

formatValue :: Value -> String
formatValue (VString s) = s
formatValue v = show v

isAs :: TypedExpr -> Bool
isAs (TypedExpr _ _ (TypedVarExpr (Ident _ "@as"))) = True
isAs _ = False

checkAsCall :: AlexPosn -> TypedExpr -> [TypedExpr] -> Check TypedExpr
checkAsCall pos callee args = case args of
  [targetTypeExpr, valueExpr] -> do
    unless (typedExprType targetTypeExpr == TypeT) $
      liftEither $
        Left $
          InvalidCast pos (typedExprType targetTypeExpr) (typedExprType valueExpr)
    case typedExprKind targetTypeExpr of
      TypedTypeLit targetType -> do
        unless (targetType /= AnyTypeT) $
          liftEither $
            Left $
              InvalidCast pos AnyTypeT (typedExprType valueExpr)
        let valueType = typedExprType valueExpr
        unless (isCastable targetType valueType) $
          liftEither $
            Left $
              InvalidCast pos targetType valueType
        return $ TypedExpr pos targetType (TypedCallExpr callee args)
      _ -> liftEither $ Left $ InvalidCast pos TypeT (typedExprType valueExpr)
  _ -> liftEither $ Left $ InvalidCast pos TypeT UnitT

isCastable :: Type -> Type -> Bool
isCastable target value = case (target, value) of
  (TypeT, TypeT) -> True
  (_, AnyTypeT) -> True
  (IntT _ _, IntT _ _) -> True
  (IntT _ _, FloatT _) -> True
  (IntT _ _, BoolT) -> True
  (IntT _ _, StringT) -> True
  (FloatT _, IntT _ _) -> True
  (FloatT _, FloatT _) -> True
  (FloatT _, BoolT) -> True
  (FloatT _, StringT) -> True
  (BoolT, BoolT) -> True
  (BoolT, IntT _ _) -> True
  (BoolT, FloatT _) -> True
  (StringT, StringT) -> True
  (StringT, BoolT) -> True
  (StringT, IntT _ _) -> True
  (StringT, FloatT _) -> True
  (StringT, UnitT) -> True
  (UnitT, UnitT) -> True
  _ -> False

dispatchAs :: AlexPosn -> [Value] -> Eval Value
dispatchAs pos = \case
  [VType targetType, value] -> castValue pos targetType value
  _ -> panicAt pos "@as expects a type and a value"

castValue :: AlexPosn -> Type -> Value -> Eval Value
castValue pos target value = case (target, value) of
  (TypeT, VType _) -> return value
  (IntT s k, VInt _ _ i) -> return $ VInt s k i
  (IntT s k, VFloat _ d) -> return $ VInt s k (truncate d)
  (IntT s k, VBool b) -> return $ VInt s k (if b then 1 else 0)
  (IntT s k, VString s') -> case readMaybe s' of
    Just i -> return $ VInt s k i
    Nothing -> throwError $ RuntimeError $ CastError pos $ "cannot cast string to '" ++ prettyType target ++ "': '" ++ s' ++ "'"
  (FloatT k, VFloat _ d) -> return $ VFloat k d
  (FloatT k, VInt _ _ i) -> return $ VFloat k (fromInteger i)
  (FloatT k, VBool b) -> return $ VFloat k (if b then 1.0 else 0.0)
  (FloatT k, VString s') -> case readMaybe s' of
    Just d -> return $ VFloat k d
    Nothing -> throwError $ RuntimeError $ CastError pos $ "cannot cast string to '" ++ prettyType target ++ "': '" ++ s' ++ "'"
  (BoolT, VBool b) -> return $ VBool b
  (BoolT, VInt _ _ i) -> return $ VBool (i /= 0)
  (BoolT, VFloat _ d) -> return $ VBool (d /= 0.0)
  (StringT, VString s) -> return $ VString s
  (StringT, VBool b) -> return $ VString (if b then "true" else "false")
  (StringT, VInt _ _ i) -> return $ VString (show i)
  (StringT, VFloat _ d) -> return $ VString (show d)
  (StringT, VUnit) -> return $ VString "()"
  (UnitT, VUnit) -> return VUnit
  _ -> panicAt pos $ "internal: unmatched @as target type '" ++ prettyType target ++ "'"


dispatchPush :: AlexPosn -> [Value] -> Eval Value
dispatchPush pos = \case
  [VArray elements, value] -> 
    return (VArray (elements ++ [value]))
  
  _ -> panicAt pos "@push recieves an array and a value" 

dispatchPop :: AlexPosn -> [Value] -> Eval Value
dispatchPop pos = \case
  [VArray []] -> 
    return (VArray [])
  [VArray elements] -> 
    return $ VArray (init elements)
  _ -> panicAt pos "@pop expects only an array"
