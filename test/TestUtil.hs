module TestUtil
  ( assertEqual,
    assertRight,
    assertLeft,
    assertOk,
    assertExpr,
    assertEvalError,
    Error (..),
    RuntimeError (..),
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
import Eval (Env, Error (..), RuntimeError (..), evalExpr, evalRepl)
import Lexer (AlexPosn (..), runAlex)
import Parser (parseRepl)
import Test.Tasty.HUnit qualified as HUnit

assertEqual :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEqual = HUnit.assertEqual

assertRight :: (Show e) => String -> Either e a -> IO a
assertRight name (Left err) = HUnit.assertFailure $ name ++ " failed with: " ++ show err
assertRight _ (Right x) = return x

assertLeft :: String -> Either e a -> IO ()
assertLeft _ (Left _) = return ()
assertLeft name (Right _) = HUnit.assertFailure $ name ++ ": expected error"

assertOk :: (Show e) => String -> Either e a -> IO ()
assertOk _ (Right _) = return ()
assertOk name (Left err) = HUnit.assertFailure $ name ++ ": expected success but got " ++ show err

assertExpr :: String -> TypedExpr -> Env -> Value -> IO ()
assertExpr name expr env expected = do
  case runExcept (runStateT (evalExpr expr) env) of
    Left err -> error $ name ++ " failed: " ++ show err
    Right (val, _) ->
      HUnit.assertEqual name expected val

assertEvalError :: String -> TypedExpr -> Env -> Error -> IO ()
assertEvalError name expr env expectedErr = do
  case runExcept (runStateT (evalExpr expr) env) of
    Left err ->
      if err == expectedErr
        then return ()
        else HUnit.assertFailure $ name ++ ": expected error '" ++ show expectedErr ++ "' but got '" ++ show err ++ "'"
    Right (val, _) -> HUnit.assertFailure $ name ++ ": expected error but got " ++ show val

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

withTokens :: String -> String -> (ParsedRepl -> IO ()) -> IO ()
withTokens name source f = do
  ast <- assertRight ("parse " ++ name) $ runAlex source parseRepl
  f ast

checkOk :: String -> String -> IO ()
checkOk name source =
  withTokens name source $ \ast ->
        case checkRepl ast of
      Right _ -> return ()
      Left err -> error $ name ++ " failed: " ++ show err

checkErr :: String -> String -> IO ()
checkErr name source =
  withTokens name source $ \ast ->
    case checkRepl ast of
      Left _ -> return ()
      Right _ -> error $ name ++ ": expected type error but passed"

evalParsed :: String -> ParsedRepl -> IO (Either Error (Value, Env))
evalParsed name ast =
  case checkRepl ast of
    Left err -> error $ name ++ " type check failed: " ++ show err
    Right typedAst -> return $ runExcept (runStateT (evalRepl typedAst) emptyEnv)
