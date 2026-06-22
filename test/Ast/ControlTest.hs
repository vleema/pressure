module Ast.ControlTest
  ( testIfExpressionEval,
    testIfStatementEval,
    testIfElseStatementEval,
    testUnaryNegEval,
    testUnaryNotEval,
  )
where

import Ast hiding (Error)
import TestUtil

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
