module Pressure.TypecheckerTest (typeTests) where

import Pressure.Language.Ast
import Pressure.Language.Lexer (runAlex)
import Pressure.Language.Parser (parseProgram)
import Pressure.Language.Types
import Pressure.TestUtil (assertEqual, assertLeft, assertOk, identFrom, pos0)
import Pressure.Typechecker (Error, checkProgram, checkRepl, checkReplWithEnv)
import Pressure.Typechecker.Check (checkExpr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

typeTests :: TestTree
typeTests =
  testGroup
    "types"
    [ testCase "checks binary expression types" testBinaryExprTypes,
      testCase "checks unary expression types" testUnaryExprTypes,
      testCase "checks if and function expression types" testIfFnExprTypes,
      testCase "checks top-level types" testTopLevelTypes,
      testCase "checks repl types" testReplTypes
    ]

intType :: Type
intType = IntT Signed I32

intSyntax :: TypeSyntax
intSyntax = TypeSyntax pos0 (IntSyntax Signed I32)

boolSyntax :: TypeSyntax
boolSyntax = TypeSyntax pos0 BoolSyntax

expr :: ParsedExprKind -> ParsedExpr
expr = ParsedExpr pos0

fnIdExpr :: ParsedExpr
fnIdExpr = expr (ParsedFnExpr [Param (identFrom "x") intSyntax] intSyntax (Block [] (Just (expr (ParsedVarExpr (identFrom "x"))))))

checkExprType :: ParsedExpr -> Either Error Type
checkExprType = fmap typedExprType . checkExpr

checkSource :: String -> Either Error ()
checkSource source = case runAlex source parseProgram of
  Left err -> error $ "parse failed: " ++ err
  Right ast -> checkProgram ast

testBinaryExprTypes :: IO ()
testBinaryExprTypes = do
  assertEqual "int addition type" (Right intType) $ checkExprType (expr (ParsedBinaryExpr AddOp (expr (ParsedIntLit 1)) (expr (ParsedIntLit 2))))
  assertLeft "bool arithmetic unsupported" $ checkExprType (expr (ParsedBinaryExpr AddOp (expr (ParsedBoolLit True)) (expr (ParsedIntLit 1))))
  assertEqual "boolean and type" (Right BoolT) $ checkExprType (expr (ParsedBinaryExpr AndOp (expr (ParsedBoolLit True)) (expr (ParsedBoolLit False))))
  assertEqual "comparison type" (Right BoolT) $ checkExprType (expr (ParsedBinaryExpr LtOp (expr (ParsedIntLit 1)) (expr (ParsedIntLit 2))))
  assertEqual "equality type" (Right BoolT) $ checkExprType (expr (ParsedBinaryExpr EqOp (expr (ParsedBoolLit True)) (expr (ParsedBoolLit False))))
  assertLeft "ordered bool comparison unsupported" $ checkExprType (expr (ParsedBinaryExpr LtOp (expr (ParsedBoolLit True)) (expr (ParsedBoolLit False))))

testUnaryExprTypes :: IO ()
testUnaryExprTypes = do
  assertEqual "unary negation type" (Right intType) $ checkExprType (expr (ParsedUnaryExpr NegOp (expr (ParsedIntLit 1))))
  assertEqual "unary not type" (Right BoolT) $ checkExprType (expr (ParsedUnaryExpr NotOp (expr (ParsedBoolLit False))))
  assertLeft "unary negation bool unsupported" $ checkExprType (expr (ParsedUnaryExpr NegOp (expr (ParsedBoolLit True))))
  assertLeft "unary not int unsupported" $ checkExprType (expr (ParsedUnaryExpr NotOp (expr (ParsedIntLit 1))))
  assertLeft "unary ampersand unsupported" $ checkExprType (expr (ParsedUnaryExpr AmpersandOp (expr (ParsedIntLit 1))))

testIfFnExprTypes :: IO ()
testIfFnExprTypes = do
  assertEqual "if expression type" (Right intType) $
    checkExprType
      ( expr
          ( ParsedIfExpr
              (expr (ParsedBoolLit True))
              (Block [] (Just (expr (ParsedIntLit 1))))
              (Just (Block [] (Just (expr (ParsedIntLit 2)))))
          )
      )
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
  assertEqual "function expression type" (Right (FnT [intType] intType)) $ checkExprType fnIdExpr
  assertLeft "calling non-function unsupported" $ checkExprType (expr (ParsedCallExpr (expr (ParsedIntLit 1)) []))
  assertLeft "function call arity mismatch" $ checkExprType (expr (ParsedCallExpr fnIdExpr []))
  assertLeft "function return type mismatch" $ checkExprType (expr (ParsedFnExpr [Param (identFrom "x") intSyntax] boolSyntax (Block [] (Just (expr (ParsedVarExpr (identFrom "x")))))))
  assertLeft "duplicate parameter names rejected" $ checkExprType (expr (ParsedFnExpr [Param (identFrom "x") intSyntax, Param (identFrom "x") intSyntax] intSyntax (Block [] (Just (expr (ParsedVarExpr (identFrom "x")))))))

testTopLevelTypes :: IO ()
testTopLevelTypes = do
  assertLeft "top-level duplicate function items rejected" $ checkSource "f :: fn(x: i32) -> i32 { x }; f :: fn(x: i32) -> i32 { x };"
  assertLeft "duplicate declarations rejected" $ checkSource "x :: 1; x :: 2;"
  assertOk "direct recursion type checks" $ checkSource "fact :: fn(n: i32) -> i32 { if n == 0 { 1 } else { n * fact(n - 1) } };"
  assertOk "mutual recursion type checks" $ checkSource "even :: fn(n: i32) -> bool { if n == 0 { true } else { odd(n - 1) } }; odd :: fn(n: i32) -> bool { if n == 0 { false } else { even(n - 1) } };"
  assertOk "forward function reference type checks" $ checkSource "result: i32 = f(); f :: fn() -> i32 { 1 };"
  assertOk "function uses preceding global" $ checkSource "x :: 10; f :: fn() -> i32 { x }; result: i32 = f();"
  assertOk "function does capture local" $ checkSource "outer :: fn(x: i32) -> i32 { helper :: fn() -> i32 { x }; helper() };"
  assertLeft "if without else cannot produce int" $
    checkProgram
      ( Program
          [ TopLevelStmt
              ( ParsedStmt
                  pos0
                  ( ParsedDeclStmt
                      ( ParsedValueDecl
                          Mutable
                          (identFrom "x")
                          (Just intSyntax)
                          (expr (ParsedIfExpr (expr (ParsedBoolLit True)) (Block [] (Just (expr (ParsedIntLit 1)))) Nothing))
                      )
                  )
              )
          ]
      )

testReplTypes :: IO ()
testReplTypes = do
  assertEqual "repl expression type checks" (Right (Repl [ReplExpr (TypedExpr pos0 intType (TypedIntLit 1))])) $ checkRepl (Repl [ReplExpr (expr (ParsedIntLit 1))])
  assertLeft "repl remembers unit variable type" $ do
    (_, env) <- checkReplWithEnv [] replUnitDecl
    checkReplWithEnv env replUnitAddition

replUnitDecl :: ParsedRepl
replUnitDecl =
  Repl
    [ ReplStmt $
        ParsedStmt pos0 $
          ParsedDeclStmt $
            ParsedValueDecl
              Mutable
              (identFrom "x")
              Nothing
              (expr (ParsedIfExpr (expr (ParsedBoolLit False)) (Block [] Nothing) Nothing))
    ]

replUnitAddition :: ParsedRepl
replUnitAddition =
  Repl
    [ ReplExpr $
        expr $
          ParsedBinaryExpr AddOp (expr (ParsedVarExpr (identFrom "x"))) (expr (ParsedIntLit 5))
    ]
