module TypeTest (testType) where

import Ast
import Lexer (AlexPosn (..), runAlex)
import Parser (parseProgram)
import TestUtil (assertEqual, assertLeft, assertOk, identFrom, pos0)

testType :: IO ()
testType = do
  testBinaryExprTypes
  testUnaryExprTypes
  testIfFnExprTypes
  testTopLevelTypes
  testReplTypes

intType :: Type
intType = IntType pos0 Signed I32

fnIdExpr :: Expr AlexPosn
fnIdExpr = Expr pos0 (FnExpr [Param (identFrom "x") intType] intType (Block [] (Just (Expr pos0 (VarExpr (identFrom "x"))))))

checkExprType :: Expr AlexPosn -> Either Error Type
checkExprType = fmap exprAnnot . checkExpr

checkSource :: String -> Either Error ()
checkSource source = case runAlex source parseProgram of
  Left err -> error $ "parse failed: " ++ err
  Right ast -> checkProgram ast

testBinaryExprTypes :: IO ()
testBinaryExprTypes = do
  assertEqual "int addition type" (Right (IntType pos0 Signed I32)) $ checkExprType (Expr pos0 (BinaryExpr AddOp (Expr pos0 (IntLit 1)) (Expr pos0 (IntLit 2))))
  assertLeft "bool arithmetic unsupported" $ checkExprType (Expr pos0 (BinaryExpr AddOp (Expr pos0 (BoolLit True)) (Expr pos0 (IntLit 1))))
  assertEqual "boolean and type" (Right (BoolType pos0)) $ checkExprType (Expr pos0 (BinaryExpr AndOp (Expr pos0 (BoolLit True)) (Expr pos0 (BoolLit False))))
  assertEqual "comparison type" (Right (BoolType pos0)) $ checkExprType (Expr pos0 (BinaryExpr LtOp (Expr pos0 (IntLit 1)) (Expr pos0 (IntLit 2))))
  assertEqual "equality type" (Right (BoolType pos0)) $ checkExprType (Expr pos0 (BinaryExpr EqOp (Expr pos0 (BoolLit True)) (Expr pos0 (BoolLit False))))
  assertLeft "ordered bool comparison unsupported" $ checkExprType (Expr pos0 (BinaryExpr LtOp (Expr pos0 (BoolLit True)) (Expr pos0 (BoolLit False))))

testUnaryExprTypes :: IO ()
testUnaryExprTypes = do
  assertEqual "unary negation type" (Right (IntType pos0 Signed I32)) $ checkExprType (Expr pos0 (UnaryExpr NegOp (Expr pos0 (IntLit 1))))
  assertEqual "unary not type" (Right (BoolType pos0)) $ checkExprType (Expr pos0 (UnaryExpr NotOp (Expr pos0 (BoolLit False))))
  assertLeft "unary negation bool unsupported" $ checkExprType (Expr pos0 (UnaryExpr NegOp (Expr pos0 (BoolLit True))))
  assertLeft "unary not int unsupported" $ checkExprType (Expr pos0 (UnaryExpr NotOp (Expr pos0 (IntLit 1))))
  assertLeft "unary ampersand unsupported" $ checkExprType (Expr pos0 (UnaryExpr AmpersandOp (Expr pos0 (IntLit 1))))

testIfFnExprTypes :: IO ()
testIfFnExprTypes = do
  assertEqual "if expression type" (Right (IntType pos0 Signed I32)) $ checkExprType (Expr pos0 (IfExpr (Expr pos0 (BoolLit True)) (Block [] (Just (Expr pos0 (IntLit 1)))) (Just (Block [] (Just (Expr pos0 (IntLit 2)))))))
  assertEqual "function expression type" (Right (FnType pos0 [intType] intType)) $ checkExprType fnIdExpr
  assertLeft "calling non-function unsupported" $ checkExprType (Expr pos0 (CallExpr (Expr pos0 (IntLit 1)) []))
  assertLeft "function call arity mismatch" $ checkExprType (Expr pos0 (CallExpr fnIdExpr []))
  assertLeft "function return type mismatch" $ checkExprType (Expr pos0 (FnExpr [Param (identFrom "x") intType] (BoolType pos0) (Block [] (Just (Expr pos0 (VarExpr (identFrom "x")))))))
  assertLeft "duplicate parameter names rejected" $ checkExprType (Expr pos0 (FnExpr [Param (identFrom "x") intType, Param (identFrom "x") intType] intType (Block [] (Just (Expr pos0 (VarExpr (identFrom "x")))))))

testTopLevelTypes :: IO ()
testTopLevelTypes = do
  assertLeft "top-level duplicate function items rejected" $ checkSource "f :: fn(x: i32) -> i32 { x }; f :: fn(x: i32) -> i32 { x };"
  assertLeft "duplicate declarations rejected" $ checkSource "x :: 1; x :: 2;"
  assertOk "direct recursion type checks" $ checkSource "fact :: fn(n: i32) -> i32 { if n == 0 { 1 } else { n * fact(n - 1) } };"
  assertOk "mutual recursion type checks" $ checkSource "even :: fn(n: i32) -> bool { if n == 0 { true } else { odd(n - 1) } }; odd :: fn(n: i32) -> bool { if n == 0 { false } else { even(n - 1) } };"
  assertOk "forward function reference type checks" $ checkSource "result: i32 = f(); f :: fn() -> i32 { 1 };"
  assertOk "function uses preceding global" $ checkSource "x :: 10; f :: fn() -> i32 { x }; result: i32 = f();"
  assertLeft "function does not capture local" $ checkSource "outer :: fn(x: i32) -> i32 { helper :: fn() -> i32 { x }; helper() };"
  assertLeft "if without else cannot produce int" $ checkProgram (Program [TopLevelStmt (Stmt pos0 (DeclStmt (ValueDecl Mutable (identFrom "x") (Just (IntType pos0 Signed I32)) (Just (Expr pos0 (IfExpr (Expr pos0 (BoolLit True)) (Block [] (Just (Expr pos0 (IntLit 1)))) Nothing))))))])
  assertLeft "missing annotation" $ checkProgram (Program [TopLevelStmt (Stmt pos0 (DeclStmt (ValueDecl Mutable (identFrom "x") Nothing Nothing)))])

testReplTypes :: IO ()
testReplTypes = do
  assertEqual "repl expression type checks" (Right (ReplExpr (Expr (IntType pos0 Signed I32) (IntLit 1)))) $ checkReplInput (ReplExpr (Expr pos0 (IntLit 1)))
  assertLeft "repl remembers unit variable type" $ do
    (_, env) <- checkReplInputWithEnv [] replUnitDecl
    checkReplInputWithEnv env replUnitAddition

replUnitDecl :: Repl AlexPosn
replUnitDecl =
  ReplStmt $
    Stmt pos0 $
      DeclStmt $
        ValueDecl
          Mutable
          (identFrom "x")
          Nothing
          (Just (Expr pos0 (IfExpr (Expr pos0 (BoolLit False)) (Block [] Nothing) Nothing)))

replUnitAddition :: Repl AlexPosn
replUnitAddition =
  ReplExpr $
    Expr pos0 $
      BinaryExpr AddOp (Expr pos0 (VarExpr (identFrom "x"))) (Expr pos0 (IntLit 5))
