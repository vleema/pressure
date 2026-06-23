module Ast.FunctionTest
  ( functionTests,
  )
where

import Ast hiding (Error)
import Ast.Typecheck (checkReplWithEnv)
import Control.Monad.Except (runExcept)
import Control.Monad.State (runStateT)
import Eval (evalRepl, evalReplInput)
import Lexer (runAlex)
import Parser (parseRepl)
import TestUtil
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

functionTests :: TestTree
functionTests =
  testGroup
    "functions"
    [ testCase "evaluates unit function sugar" testUnitFunctionSugar,
      testCase "evaluates functions" testFunctionEval,
      testCase "rejects same-block closure capture" testClosureCapturesByValue,
      testCase "keeps function scope local" testFunctionLocalScope,
      testCase "evaluates direct recursion" testDirectRecursion,
      testCase "evaluates top-level mutual recursion" testTopLevelMutualRecursion,
      testCase "rejects local mutual recursion" testLocalMutualRecursionRejected,
      testCase "evaluates forward function references" testForwardFunctionReference,
      testCase "evaluates function global access" testFunctionUsesGlobal,
      testCase "evaluates repl recursive functions" testReplRecursiveFunction,
      testCase "rejects nested function capture" testNestedFunctionCaptureRejected
    ]

testUnitFunctionSugar :: IO ()
testUnitFunctionSugar = do
  withTokens "unit funcition sugar" "sugar :: fn() {}; result: unit  = sugar();" $ \ast -> do
    result <- evalParsed "unit function sugar" ast
    case result of
      Left err -> error $ "eval failed: " ++ show err
      Right (_, env) ->
        case lookupValue "result" env of
          Just VUnit -> return ()
          other -> error $ "expected result = (), got " ++ show other

testFunctionEval :: IO ()
testFunctionEval = do
  withTokens "function eval" "add :: fn(a: i32, b: i32) -> i32 { a + b }; result: i32 = add(1, 2);" $ \ast -> do
    result <- evalParsed "function eval" ast
    case result of
      Right (_, env) ->
        case lookupValue "result" env of
          Just (VInt Signed I32 3) -> return ()
          other -> error $ "expected result = 3, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testClosureCapturesByValue :: IO ()
testClosureCapturesByValue = do
  checkErr "closure does not capture same-block variable" "x :: 10; addX :: fn(y: i32) -> i32 { x + y }; x :: 20; result: i32 = addX(5);"

testFunctionLocalScope :: IO ()
testFunctionLocalScope = do
  withTokens "function local scope" "f :: fn() -> i32 { x :: 1; x }; result: i32 = f();" $ \ast -> do
    result <- evalParsed "function local scope" ast
    case result of
      Right (_, env) -> do
        case lookupValue "result" env of
          Just (VInt Signed I32 1) -> return ()
          other -> error $ "expected result = 1, got " ++ show other
        case lookupValue "x" env of
          Nothing -> return ()
          other -> error $ "expected function local x to be absent, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testDirectRecursion :: IO ()
testDirectRecursion = do
  withTokens "direct recursion" "fact :: fn(n: i32) -> i32 { if n == 0 { 1 } else { n * fact(n - 1) } }; result: i32 = fact(5);" $ \ast -> do
    result <- evalParsed "direct recursion" ast
    case result of
      Right (_, env) ->
        case lookupValue "result" env of
          Just (VInt Signed I32 120) -> return ()
          other -> error $ "expected result = 120, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testTopLevelMutualRecursion :: IO ()
testTopLevelMutualRecursion = do
  withTokens "top-level mutual recursion" "even :: fn(n: i32) -> bool { if n == 0 { true } else { odd(n - 1) } }; odd :: fn(n: i32) -> bool { if n == 0 { false } else { even(n - 1) } }; result: bool = even(10);" $ \ast -> do
    result <- evalParsed "top-level mutual recursion" ast
    case result of
      Right (_, env) ->
        case lookupValue "result" env of
          Just (VBool True) -> return ()
          other -> error $ "expected result = true, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testLocalMutualRecursionRejected :: IO ()
testLocalMutualRecursionRejected = do
  checkErr "local mutual recursion rejected" "outer :: fn(n: i32) -> bool { even :: fn(x: i32) -> bool { if x == 0 { true } else { odd(x - 1) } }; odd :: fn(x: i32) -> bool { if x == 0 { false } else { even(x - 1) } }; even(n) }; result: bool = outer(9);"

testForwardFunctionReference :: IO ()
testForwardFunctionReference = do
  withTokens "forward function reference" "result: i32 = f(); f :: fn() -> i32 { 42 };" $ \ast -> do
    result <- evalParsed "forward function reference" ast
    case result of
      Right (_, env) ->
        case lookupValue "result" env of
          Just (VInt Signed I32 42) -> return ()
          other -> error $ "expected result = 42, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testFunctionUsesGlobal :: IO ()
testFunctionUsesGlobal = do
  withTokens "function uses global" "x :: 10; f :: fn() -> i32 { x }; result: i32 = f();" $ \ast -> do
    result <- evalParsed "function uses global" ast
    case result of
      Right (_, env) ->
        case lookupValue "result" env of
          Just (VInt Signed I32 10) -> return ()
          other -> error $ "expected result = 10, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testReplRecursiveFunction :: IO ()
testReplRecursiveFunction = do
  decl <- assertRight "parse repl recursive function" $ runAlex "succ :: fn(n:int) -> int {if n == 0 { 1 } else { 1 + succ(n-1) }}" parseRepl
  expr <- assertRight "parse repl recursive call" $ runAlex "succ(3)" parseRepl
  case checkReplWithEnv [] decl of
    Left err -> error $ "repl recursive function type check failed: " ++ show err
    Right (typedDecl, typeEnv) ->
      case runExcept (runStateT (evalRepl typedDecl) emptyEnv) of
        Left err -> error $ "repl recursive function eval failed: " ++ show err
        Right (_, env) ->
          case checkReplWithEnv typeEnv expr of
            Left err -> error $ "repl recursive call type check failed: " ++ show err
            Right (Repl [typedExpr], _) ->
              case runExcept (runStateT (evalReplInput typedExpr) env) of
                Right (VInt Signed I32 4, _) -> return ()
                Right (val, _) -> error $ "expected succ(3) = 4, got " ++ show val
                Left err -> error $ "repl recursive call eval failed: " ++ show err
            Right (Repl other, _) -> error $ "expected single expression, got " ++ show (length other) ++ " inputs"

testNestedFunctionCaptureRejected :: IO ()
testNestedFunctionCaptureRejected = do
  checkErr "nested function capture rejected" "outer :: fn(x: i32) -> i32 { helper :: fn(n: i32) -> i32 { if n == 0 { x } else { helper(n - 1) } }; helper(3) }; result: i32 = outer(7);"
