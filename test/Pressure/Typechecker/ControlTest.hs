module Pressure.Typechecker.ControlTest (controlTypeTests) where

import Pressure.Language.Ast
import Pressure.Language.Types
import Pressure.TestUtil (assertEqual, assertLeft, checkErr, checkOk, identFrom, pos0)
import Pressure.Typechecker.Check (checkExpr)
import Pressure.Typechecker.Error (Error)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

controlTypeTests :: TestTree
controlTypeTests =
  testGroup
    "control"
    [ testCase "checks empty while" testEmptyWhile,
      testCase "checks while break" testWhileBreak,
      testCase "rejects missing loop else" testWhileElseFalse,
      testCase "checks if expression type" testIfExprType,
      testCase "checks else-if desugared expression type" testElseIfExprType,
      testCase "checks else-if syntactic sugar" testElseIfSugar,
      testCase "checks function expression type" testFnExprType,
      testCase "rejects calling non-function" testCallNonFnError,
      testCase "rejects function call arity mismatch" testCallArityMismatch,
      testCase "rejects function return type mismatch" testFnReturnTypeMismatch,
      testCase "rejects duplicate parameter names" testDuplicateParams
    ]

intType :: Type
intType = IntT Signed I32

intSyntax :: TypeSyntax
intSyntax = TypeSyntax pos0 (IntSyntax Signed I32)

boolSyntax :: TypeSyntax
boolSyntax = TypeSyntax pos0 BoolSyntax

expr :: ParsedExprKind -> ParsedExpr
expr = ParsedExpr pos0

checkExprType :: ParsedExpr -> Either Error Type
checkExprType = fmap typedExprType . checkExpr

testEmptyWhile :: IO ()
testEmptyWhile = checkOk "empty while" "while true { };"

testWhileBreak :: IO ()
testWhileBreak = checkOk "while with only break" "while true { break; };"

testWhileElseFalse :: IO ()
testWhileElseFalse = checkErr "while else without break" "x: i32 = while false {  } else { 42 };"

testIfExprType :: IO ()
testIfExprType =
  assertEqual "if expression type" (Right intType) $
    checkExprType
      ( expr
          ( ParsedIfExpr
              (expr (ParsedBoolLit True))
              (Block [] (Just (expr (ParsedIntLit 1))))
              (Just (Block [] (Just (expr (ParsedIntLit 2)))))
          )
      )

testElseIfExprType :: IO ()
testElseIfExprType =
  assertEqual "else if desugared expression type" (Right intType) $
    checkExprType
      ( expr
          ( ParsedIfExpr
              (expr (ParsedBoolLit True))
              (Block [] (Just (expr (ParsedIntLit 1))))
              ( Just
                  ( Block
                      []
                      ( Just
                          ( expr
                              ( ParsedIfExpr
                                  (expr (ParsedBoolLit False))
                                  (Block [] (Just (expr (ParsedIntLit 2))))
                                  (Just (Block [] (Just (expr (ParsedIntLit 3)))))
                              )
                          )
                      )
                  )
              )
          )
      )

testElseIfSugar :: IO ()
testElseIfSugar = checkOk "else if syntactic sugar type checks" "foo :: fn(x: i32) -> i32 { if x == 1 { 10 } else if x == 2 { 20 } else { 30 } };"

testFnExprType :: IO ()
testFnExprType =
  assertEqual "function expression type" (Right (FnT [intType] intType)) $
    checkExprType
      (expr (ParsedFnExpr [Param (identFrom "x") intSyntax] intSyntax (Block [] (Just (expr (ParsedVarExpr (identFrom "x")))))))

testCallNonFnError :: IO ()
testCallNonFnError =
  assertLeft "calling non-function unsupported" $ checkExprType (expr (ParsedCallExpr (expr (ParsedIntLit 1)) []))

testCallArityMismatch :: IO ()
testCallArityMismatch =
  assertLeft "function call arity mismatch" $
    checkExprType
      ( expr
          ( ParsedCallExpr
              (expr (ParsedFnExpr [Param (identFrom "x") intSyntax] intSyntax (Block [] (Just (expr (ParsedVarExpr (identFrom "x")))))))
              []
          )
      )

testFnReturnTypeMismatch :: IO ()
testFnReturnTypeMismatch =
  assertLeft "function return type mismatch" $
    checkExprType
      ( expr
          ( ParsedFnExpr
              [Param (identFrom "x") intSyntax]
              boolSyntax
              (Block [] (Just (expr (ParsedVarExpr (identFrom "x")))))
          )
      )

testDuplicateParams :: IO ()
testDuplicateParams =
  assertLeft "duplicate parameter names rejected" $
    checkExprType
      ( expr
          ( ParsedFnExpr
              [Param (identFrom "x") intSyntax, Param (identFrom "x") intSyntax]
              intSyntax
              (Block [] (Just (expr (ParsedVarExpr (identFrom "x")))))
          )
      )
