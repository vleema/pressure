module Ast.ErrorTest
  ( testBoolInArithmeticError,
    testBoolInArithmeticRightError,
    testTypeMismatchError,
    testFloatNarrowingError,
    testVarUndefined,
    testMissingAnnotationError,
    testDuplicateParamsRejected,
    testDuplicateFunctionsRejected,
    testDuplicateDeclarationsRejected,
  )
where

import Ast hiding (Error)
import TestUtil

testBoolInArithmeticError :: IO ()
testBoolInArithmeticError = checkErr "bool in arithmetic" "x: int = true + 1;"

testBoolInArithmeticRightError :: IO ()
testBoolInArithmeticRightError = checkErr "bool on right of arithmetic" "x: int = 1 + true;"

testTypeMismatchError :: IO ()
testTypeMismatchError = checkErr "type mismatch" "x: bool = 42;"

testFloatNarrowingError :: IO ()
testFloatNarrowingError = checkErr "float to int narrowing" "x: int = 3.14;"

testVarUndefined :: IO ()
testVarUndefined =
  assertEvalError "undefined variable" (Expr UnitType (VarExpr (identFrom "z"))) emptyEnv (RuntimeError "undefined variable: z")

testMissingAnnotationError :: IO ()
testMissingAnnotationError =
  case checkProgram (Program [TopLevelStmt (Stmt pos0 (DeclStmt (ValueDecl Mutable (identFrom "x") Nothing Nothing)))]) of
    Left _ -> return ()
    Right () -> error "missing annotation: expected type error but passed"

testDuplicateParamsRejected :: IO ()
testDuplicateParamsRejected = do
  checkErr "duplicate params" "f :: fn(x: i32, x: i32) -> i32 { x };"

testDuplicateFunctionsRejected :: IO ()
testDuplicateFunctionsRejected = do
  checkErr "duplicate functions" "f :: fn() -> i32 { 1 }; f :: fn() -> i32 { 2 };"

testDuplicateDeclarationsRejected :: IO ()
testDuplicateDeclarationsRejected = do
  checkErr "duplicate declarations" "x :: 1; x :: 2;"
