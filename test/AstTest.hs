module AstTest (testAst) where

import Ast.ArithTest
import Ast.ControlTest
import Ast.ErrorTest
import Ast.FunctionTest
import Ast.LiteralTest

testAst :: IO ()
testAst = do
  testIntLit
  testFloatLit
  testBoolLit
  testIntAdd
  testFloatAdd
  testIntDiv
  testFloatDiv
  testDivByZero
  testTypeNameAnnotation
  testBoolInArithmeticError
  testBoolInArithmeticRightError
  testTypeMismatchError
  testFloatNarrowingError
  testVarDeclAndLookup
  testUndefinedVariableTypeError
  testVarDefaultValue
  testIntMul
  testFloatMul
  testIntSub
  testFloatSub
  testMixedSubEval
  testBoolDefaultValue
  testMissingAnnotationError
  testIfExpressionEval
  testIfStatementEval
  testIfElseStatementEval
  testUnaryNegEval
  testUnaryNotEval
  testUnitFunctionSugar
  testFunctionEval
  testClosureCapturesByValue
  testFunctionLocalScope
  testDirectRecursion
  testTopLevelMutualRecursion
  testLocalMutualRecursionRejected
  testForwardFunctionReference
  testFunctionUsesGlobal
  testReplRecursiveFunction
  testDuplicateParamsRejected
  testDuplicateFunctionsRejected
  testDuplicateDeclarationsRejected
  testNestedFunctionCaptureRejected
  testTypeErrorMessageFormat
  testRuntimeErrorMessageFormat
