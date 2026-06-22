module Ast.ArithTest
  ( testIntAdd,
    testFloatAdd,
    testIntDiv,
    testFloatDiv,
    testDivByZero,
    testTypeNameAnnotation,
    testIntMul,
    testFloatMul,
    testIntSub,
    testFloatSub,
    testMixedSubEval,
  )
where

import Ast hiding (Error)
import TestUtil

testIntAdd :: IO ()
testIntAdd = checkOk "int addition" "x: int = 1 + 2;"

testFloatAdd :: IO ()
testFloatAdd = checkOk "float addition" "x: float = 1.0 + 2.0;"

testIntDiv :: IO ()
testIntDiv = checkOk "int division" "x: int = 8 / 4;"

testFloatDiv :: IO ()
testFloatDiv = checkOk "float division" "x: float = 3.0 / 2.0;"

testDivByZero :: IO ()
testDivByZero = do
  withTokens "division by zero int" "x: int = 1 / 0;" $ \ast -> do
    result <- evalParsed "division by zero int" ast
    case result of
      Left (RuntimeError "division by zero") -> return ()
      Left err -> error $ "expected 'division by zero' got '" ++ show err ++ "'"
      Right _ -> error "expected runtime error for division by zero"

  withTokens "division by zero float" "x: float = 1.0 / 0.0;" $ \ast -> do
    result <- evalParsed "division by zero float" ast
    case result of
      Left (RuntimeError "division by zero") -> return ()
      Left err -> error $ "expected 'division by zero' got '" ++ show err ++ "'"
      Right _ -> error "expected runtime error for division by zero"

testTypeNameAnnotation :: IO ()
testTypeNameAnnotation = checkOk "TypeName annotation" "x: i32 = 42;"

testIntMul :: IO ()
testIntMul = checkOk "int multiplication" "x: int = 3 * 4;"

testFloatMul :: IO ()
testFloatMul = checkOk "float multiplication" "x: float = 1.5 * 2.0;"

testIntSub :: IO ()
testIntSub = checkOk "int subtraction" "x: int = 8 - 3;"

testFloatSub :: IO ()
testFloatSub = checkOk "float subtraction" "x: float = 8.5 - 3.0;"

testMixedSubEval :: IO ()
testMixedSubEval =
  assertExpr
    "float subtraction eval"
    (Expr UnitType (BinaryExpr SubOp (Expr UnitType (FloatLit 8.5)) (Expr UnitType (FloatLit 3.0))))
    emptyEnv
    (VFloat F64 5.5)
