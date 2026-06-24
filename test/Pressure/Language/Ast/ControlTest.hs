module Pressure.Language.Ast.ControlTest
  ( controlTests,
  )
where

import Pressure.Interpreter.Value (Value (..))
import Pressure.Language.Types
import Pressure.TestUtil
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

controlTests :: TestTree
controlTests =
  testGroup
    "control"
    [ testCase "evaluates if expressions" testIfExpressionEval,
      testCase "evaluates if statements" testIfStatementEval,
      testCase "evaluates if-else statements" testIfElseStatementEval,
      testCase "evaluates unary negation" testUnaryNegEval,
      testCase "evaluates unary not" testUnaryNotEval,
      testCase "evaluates while else on false" testWhileElseFalse,
      testCase "checks empty while" testEmptyWhile,
      testCase "checks while break" testWhileBreak,
      testCase "evaluates while else on true with break" testWhileBreakValue,
      testCase "evaluates while as statement" testWhileStatement,
      testCase "evaluates while continue" testWhileContinue,
      testCase "evaluates nested while loops" testNestedWhile
    ]

testIfExpressionEval :: IO ()
testIfExpressionEval = do
  withTokens "if expression eval" "x: int = if true { 1 } else { 2 };" $ \ast -> do
    result <- evalParsed "if expression eval" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VInt Signed I32 1) -> return ()
          other -> error $ "expected x = 1, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testIfStatementEval :: IO ()
testIfStatementEval = do
  withTokens "if statement eval" "if false { x: int = 1; }" $ \ast -> do
    result <- evalParsed "if statement eval" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Nothing -> return ()
          other -> error $ "expected x to be absent, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testIfElseStatementEval :: IO ()
testIfElseStatementEval = do
  withTokens "if else statement eval" "if false { x: int = 1; } else { y: float = 42.0; }" $ \ast -> do
    result <- evalParsed "if else statement eval" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Nothing -> return ()
          other -> error $ "expected x to be absent, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testUnaryNegEval :: IO ()
testUnaryNegEval = do
  withTokens "unary negation eval" "x: int = -42;" $ \ast -> do
    result <- evalParsed "unary negation eval" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VInt Signed I32 (-42)) -> return ()
          other -> error $ "expected x = -42, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testUnaryNotEval :: IO ()
testUnaryNotEval = do
  withTokens "unary not eval" "x: bool = !false;" $ \ast -> do
    result <- evalParsed "unary not eval" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VBool True) -> return ()
          other -> error $ "expected x = true, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testEmptyWhile :: IO ()
testEmptyWhile = do
  checkOk "empty while" "while true { };"

testWhileBreak :: IO ()
testWhileBreak = do
  checkOk "while with only break" "while true { break; };"

testWhileElseFalse :: IO ()
testWhileElseFalse = do
  checkErr "while else without break" "x: i32 = while false {  } else { 42 };"

testWhileBreakValue :: IO ()
testWhileBreakValue = do
  withTokens "while break value" "x: i32 = while true { break 10; } else { 0 };" $ \ast -> do
    result <- evalParsed "while break value" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VInt Signed I32 10) -> return ()
          other -> error $ "expected x = 10, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testWhileStatement :: IO ()
testWhileStatement = do
  withTokens "while as statement" "x: i32 = while true { break 1; } else { 2 };" $ \ast -> do
    result <- evalParsed "while as statement" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VInt Signed I32 1) -> return ()
          other -> error $ "expected x = 1, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testWhileContinue :: IO ()
testWhileContinue = do
  withTokens "while continue" "x: i32 = while true { if false { continue; }; break 1; } else { 2 };" $ \ast -> do
    result <- evalParsed "while continue" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VInt Signed I32 1) -> return ()
          other -> error $ "expected x = 1, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testNestedWhile :: IO ()
testNestedWhile = do
  withTokens "nested while" "x: i32 = while true { inner: i32 = while true { break 1; } else { 0 }; break inner; } else { 0 };" $ \ast -> do
    result <- evalParsed "nested while" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VInt Signed I32 1) -> return ()
          other -> error $ "expected x = 1, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err
