module Ast.ErrorTest
  ( errorTests,
  )
where

import Ast hiding (Error, UndefinedVariable)
import Ast.Typecheck qualified as T
import Eval qualified
import Lexer (AlexPosn (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestUtil

errorTests :: TestTree
errorTests =
  testGroup
    "errors"
    [ testCase "rejects bool on left of arithmetic" testBoolInArithmeticError,
      testCase "rejects bool on right of arithmetic" testBoolInArithmeticRightError,
      testCase "rejects type mismatches" testTypeMismatchError,
      testCase "rejects float narrowing" testFloatNarrowingError,
      testCase "rejects undefined variables" testUndefinedVariableTypeError,
      testCase "rejects missing annotations" testMissingAnnotationError,
      testCase "rejects duplicate parameters" testDuplicateParamsRejected,
      testCase "rejects duplicate functions" testDuplicateFunctionsRejected,
      testCase "rejects duplicate declarations" testDuplicateDeclarationsRejected,
      testCase "rejects undefined types" testUndefinedTypeError,
      testCase "formats type errors" testTypeErrorMessageFormat,
      testCase "formats runtime errors" testRuntimeErrorMessageFormat,
      testCase "rejects break outside loop" testBreakOutsideLoop,
      testCase "rejects continue outside loop" testContinueOutsideLoop,
      testCase "rejects missing loop else" testMissingLoopElse,
      testCase "rejects non-unit loop body" testNonUnitLoopBody,
      testCase "rejects break value type mismatch" testBreakValueMismatch
    ]

testBoolInArithmeticError :: IO ()
testBoolInArithmeticError = checkErr "bool in arithmetic" "x: int = true + 1;"

testBoolInArithmeticRightError :: IO ()
testBoolInArithmeticRightError = checkErr "bool on right of arithmetic" "x: int = 1 + true;"

testTypeMismatchError :: IO ()
testTypeMismatchError = checkErr "type mismatch" "x: bool = 42;"

testFloatNarrowingError :: IO ()
testFloatNarrowingError = checkErr "float to int narrowing" "x: int = 3.14;"

testUndefinedVariableTypeError :: IO ()
testUndefinedVariableTypeError = checkErr "undefined variable" "x: int = y;"

testMissingAnnotationError :: IO ()
testMissingAnnotationError =
  case checkProgram (Program [TopLevelStmt (ParsedStmt pos0 (ParsedDeclStmt (ParsedValueDecl Mutable (identFrom "x") Nothing Nothing)))]) of
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

testUndefinedTypeError :: IO ()
testUndefinedTypeError = do
  checkErr "undefined type in function return" "foo :: fn() -> bar {};"
  checkErr "undefined type in annotation" "x: baz = 1;"
  checkErr "undefined type in function param" "f :: fn(x: qux) -> i32 { 1 };"
  checkErr "undefined type in function item return" "f :: fn() -> baz {};"

testTypeErrorMessageFormat :: IO ()
testTypeErrorMessageFormat = do
  let pos = AlexPn 0 1 10
  let (p, m) = T.errorInfo (T.TypeMismatch pos (IntT Signed I32) BoolT)
  assertEqual "type mismatch pos" pos p
  assertEqual "type mismatch text" "type mismatch: expected 'i32', found 'bool'" m

  let (p2, m2) = T.errorInfo (T.UnsupportedOp pos AddOp (IntT Signed I32) BoolT)
  assertEqual "unsupported op pos" pos p2
  assertEqual "unsupported op text" "cannot use operator '+' on type 'i32' and 'bool'" m2

  let (p3, m3) = T.errorInfo (T.UndefinedVariable pos "foo")
  assertEqual "type undefined pos" pos p3
  assertEqual "type undefined text" "undefined variable 'foo'" m3

  let (p4, m4) = T.errorInfo (T.UndefinedType pos "bar")
  assertEqual "type undefined type pos" pos p4
  assertEqual "type undefined type text" "undefined type 'bar'" m4

testRuntimeErrorMessageFormat :: IO ()
testRuntimeErrorMessageFormat = do
  let pos = AlexPn 0 1 10
  let (mPos, m) = Eval.errorInfo (RuntimeError (DivisionByZero pos))
  assertEqual "runtime div by zero pos" (Just pos) mPos
  assertEqual "runtime div by zero text" "division by zero" m

testBreakOutsideLoop :: IO ()
testBreakOutsideLoop = checkErr "break outside loop" "break;"

testContinueOutsideLoop :: IO ()
testContinueOutsideLoop = checkErr "continue outside loop" "continue;"

testMissingLoopElse :: IO ()
testMissingLoopElse = checkErr "missing loop else" "x: i32 = while true { break 1; };"

testNonUnitLoopBody :: IO ()
testNonUnitLoopBody = checkErr "non-unit loop body" "x: i32 = while true { 42 } else { 0 };"

testBreakValueMismatch :: IO ()
testBreakValueMismatch = checkErr "break value mismatch" "x: i32 = while true { break 1; } else { true };"
