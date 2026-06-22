module Ast.AssignTest
  ( assignTests,
  )
where

import Ast hiding (Error)
import TestUtil
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

assignTests :: TestTree
assignTests =
  testGroup
    "assignment"
    [ testCase "mutates mutable variables" testMutableAssign,
      testCase "handles multiple assignments" testMultipleAssign,
      testCase "assigns in nested block" testAssignInBlock,
      testCase "rejects assignment to constants" testAssignToConstant,
      testCase "rejects assignment to undefined" testAssignToUndefined,
      testCase "rejects assignment type mismatch" testAssignTypeMismatch,
      testCase "assigns with expression" testAssignWithExpr,
      testCase "compound add" testCompoundAdd,
      testCase "compound subtract" testCompoundSub,
      testCase "compound multiply" testCompoundMul,
      testCase "compound divide int" testCompoundDivInt,
      testCase "compound divide float" testCompoundDivFloat
    ]

testMutableAssign :: IO ()
testMutableAssign =
  withTokens "mutable assign" "x: int = 42; x = 10;" $ \ast -> do
    result <- evalParsed "mutable assign" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VInt Signed I32 10) -> return ()
          other -> error $ "expected x = 10, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testMultipleAssign :: IO ()
testMultipleAssign =
  withTokens "multiple assign" "x: int = 1; x = 2; x = 3;" $ \ast -> do
    result <- evalParsed "multiple assign" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VInt Signed I32 3) -> return ()
          other -> error $ "expected x = 3, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testAssignInBlock :: IO ()
testAssignInBlock =
  withTokens "assign in block" "x: int = 1; if true { x = 2; }" $ \ast -> do
    result <- evalParsed "assign in block" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VInt Signed I32 2) -> return ()
          other -> error $ "expected x = 2, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testAssignToConstant :: IO ()
testAssignToConstant = checkErr "assign to constant" "x :: 42; x = 10;"

testAssignToUndefined :: IO ()
testAssignToUndefined = checkErr "assign to undefined" "x = 10;"

testAssignTypeMismatch :: IO ()
testAssignTypeMismatch = checkErr "assign type mismatch" "x: bool = true; x = 42;"

testAssignWithExpr :: IO ()
testAssignWithExpr =
  withTokens "assign with expr" "x: int = 5; x = x + 1;" $ \ast -> do
    result <- evalParsed "assign with expr" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VInt Signed I32 6) -> return ()
          other -> error $ "expected x = 6, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testCompoundAdd :: IO ()
testCompoundAdd =
  withTokens "compound add" "x: int = 5; x += 3;" $ \ast -> do
    result <- evalParsed "compound add" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VInt Signed I32 8) -> return ()
          other -> error $ "expected x = 8, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testCompoundSub :: IO ()
testCompoundSub =
  withTokens "compound sub" "x: int = 10; x -= 3;" $ \ast -> do
    result <- evalParsed "compound sub" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VInt Signed I32 7) -> return ()
          other -> error $ "expected x = 7, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testCompoundMul :: IO ()
testCompoundMul =
  withTokens "compound mul" "x: int = 6; x *= 3;" $ \ast -> do
    result <- evalParsed "compound mul" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VInt Signed I32 18) -> return ()
          other -> error $ "expected x = 18, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testCompoundDivInt :: IO ()
testCompoundDivInt =
  withTokens "compound div int" "x: int = 10; x /= 3;" $ \ast -> do
    result <- evalParsed "compound div int" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VInt Signed I32 3) -> return ()
          other -> error $ "expected x = 3, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testCompoundDivFloat :: IO ()
testCompoundDivFloat =
  withTokens "compound div float" "x: float = 10.0; x /= 3.0;" $ \ast -> do
    result <- evalParsed "compound div float" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VFloat F64 v) ->
            if abs (v - 3.3333333333333335) < 0.0001
              then return ()
              else error $ "expected x ≈ 3.333, got " ++ show v
          other -> error $ "expected float, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err
