module Ast.LiteralTest
  ( testIntLit,
    testFloatLit,
    testBoolLit,
    testVarDeclAndLookup,
    testVarDefaultValue,
    testBoolDefaultValue,
  )
where

import Ast hiding (Error)
import TestUtil

testIntLit :: IO ()
testIntLit = do
  checkOk "int literal" "x: int = 42;"
  withTokens "int literal eval" "x: int = 42;" $ \ast -> do
    result <- evalParsed "int literal eval" ast
    case result of
      Left err -> error $ "int literal eval failed: " ++ show err
      Right (val, env) -> do
        if val == VUnit then return () else error $ "expected VUnit got " ++ show val
        case lookupValue "x" env of
          Just (VInt Signed I32 42) -> return ()
          other -> error $ "expected x = 42, got " ++ show other

testFloatLit :: IO ()
testFloatLit = do
  checkOk "float literal" "x: float = 3.14;"
  withTokens "float literal eval" "x: float = 3.14;" $ \ast -> do
    result <- evalParsed "float literal eval" ast
    case result of
      Left err -> error $ "float literal eval failed: " ++ show err
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VFloat F64 3.14) -> return ()
          other -> error $ "expected 3.14, got " ++ show other

testBoolLit :: IO ()
testBoolLit = do
  checkOk "bool literal" "x: bool = true;"
  withTokens "bool literal eval" "x: bool = true;" $ \ast -> do
    result <- evalParsed "bool literal eval" ast
    case result of
      Left err -> error $ "bool literal eval failed: " ++ show err
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VBool True) -> return ()
          other -> error $ "expected true, got " ++ show other

testVarDeclAndLookup :: IO ()
testVarDeclAndLookup = do
  let decl42 = "x: int = 42;"
  withTokens "parse decl42" decl42 $ \ast -> do
    result <- evalParsed "var decl and lookup" ast
    case result of
      Right (_, env) ->
        assertExpr "x after decl" (Expr UnitType (VarExpr (identFrom "x"))) env (VInt Signed I32 42)
      Left err -> error $ "eval failed: " ++ show err

testVarDefaultValue :: IO ()
testVarDefaultValue = do
  withTokens "decl without init" "x: int;" $ \ast -> do
    result <- evalParsed "decl without init" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VInt Signed I32 0) -> return ()
          other -> error $ "expected x = 0 for uninitialized int, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testBoolDefaultValue :: IO ()
testBoolDefaultValue = do
  withTokens "bool decl without init" "x: bool;" $ \ast -> do
    result <- evalParsed "bool decl without init" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VBool False) -> return ()
          other -> error $ "expected x = false for uninitialized bool, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err
