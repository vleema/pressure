module TestUtil
  ( assertEqual,
    assertRight,
    assertLeft,
    assertOk,
    assertExpr,
    assertEvalError,
    Error (..),
    pos0,
    identFrom,
    emptyEnv,
    lookupValue,
    withTokens,
    checkOk,
    checkErr,
    evalParsed,
  )
where

import Ast hiding (Error)
import Control.Monad.Except (runExcept)
import Control.Monad.State (runStateT)
import Data.Map.Strict qualified as Map
import Eval (Env, Error (..), evalExpr, evalProgram)
import Lexer (AlexPosn (..), runAlex)
import Parser (parseProgram)

assertEqual :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEqual name expected actual =
  if expected == actual
    then return ()
    else error $ name ++ " failed:\n  expected: " ++ show expected ++ "\n  actual:   " ++ show actual

assertRight :: Show e => String -> Either e a -> IO a
assertRight name (Left err) = error $ name ++ " failed with: " ++ show err
assertRight _ (Right x) = return x

assertLeft :: String -> Either e a -> IO ()
assertLeft _ (Left _) = return ()
assertLeft name (Right _) = error $ name ++ ": expected error"

assertOk :: Show e => String -> Either e a -> IO ()
assertOk _ (Right _) = return ()
assertOk name (Left err) = error $ name ++ ": expected success but got " ++ show err

assertExpr :: String -> Expr Type -> Env -> Value -> IO ()
assertExpr name expr env expected = do
  case runExcept (runStateT (evalExpr expr) env) of
    Left err -> error $ name ++ " failed: " ++ show err
    Right (val, _) ->
      if val == expected
        then return ()
        else error $ name ++ ": expected " ++ show expected ++ " but got " ++ show val

assertEvalError :: String -> Expr Type -> Env -> Error -> IO ()
assertEvalError name expr env expectedErr = do
  case runExcept (runStateT (evalExpr expr) env) of
    Left err ->
      if err == expectedErr
        then return ()
        else error $ name ++ ": expected error '" ++ show expectedErr ++ "' but got '" ++ show err ++ "'"
    Right (val, _) -> error $ name ++ ": expected error but got " ++ show val

pos0 :: AlexPosn
pos0 = AlexPn 0 1 1

identFrom :: String -> Ident
identFrom name = Ident pos0 name

emptyEnv :: Env
emptyEnv = []

lookupValue :: String -> Env -> Maybe Value
lookupValue _ [] = Nothing
lookupValue name (scope : rest) =
  case Map.lookup name scope of
    Just v -> Just v
    Nothing -> lookupValue name rest

withTokens :: String -> String -> (ParsedProgram -> IO ()) -> IO ()
withTokens name source f = do
  ast <- assertRight ("parse " ++ name) $ runAlex source parseProgram
  f ast

checkOk :: String -> String -> IO ()
checkOk name source =
  withTokens name source $ \ast ->
    case checkProgram ast of
      Right () -> return ()
      Left err -> error $ name ++ " failed: " ++ show err

checkErr :: String -> String -> IO ()
checkErr name source =
  withTokens name source $ \ast ->
    case checkProgram ast of
      Left _ -> return ()
      Right () -> error $ name ++ ": expected type error but passed"

evalParsed :: String -> ParsedProgram -> IO (Either Error (Value, Env))
evalParsed name ast =
  case checkProgramTyped ast of
    Left err -> error $ name ++ " type check failed: " ++ show err
    Right typedAst -> return $ runExcept (runStateT (evalProgram typedAst) emptyEnv)
