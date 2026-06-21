module TypeTest (testType) where

import Ast
import Data.Map.Strict qualified as Map
import Lexer (AlexPosn (..))

checkExprType :: Expr AlexPosn -> Either Error Type
checkExprType = fmap exprAnnot . checkExpr

testType :: IO ()
testType = do
  let intType = IntType pos0 Signed I32
  let fnIdExpr = Expr pos0 (FnExpr [Param (identFrom "x") intType] intType (Block [] (Just (Expr pos0 (VarExpr (identFrom "x"))))))
  assertEqual "int addition type" (Right (IntType pos0 Signed I32)) $ checkExprType (Expr pos0 (BinaryExpr AddOp (Expr pos0 (IntLit 1)) (Expr pos0 (IntLit 2))))
  assertLeft "bool arithmetic unsupported" $ checkExprType (Expr pos0 (BinaryExpr AddOp (Expr pos0 (BoolLit True)) (Expr pos0 (IntLit 1))))
  assertEqual "boolean and type" (Right (BoolType pos0)) $ checkExprType (Expr pos0 (BinaryExpr AndOp (Expr pos0 (BoolLit True)) (Expr pos0 (BoolLit False))))
  assertEqual "comparison type" (Right (BoolType pos0)) $ checkExprType (Expr pos0 (BinaryExpr LtOp (Expr pos0 (IntLit 1)) (Expr pos0 (IntLit 2))))
  assertEqual "equality type" (Right (BoolType pos0)) $ checkExprType (Expr pos0 (BinaryExpr EqOp (Expr pos0 (BoolLit True)) (Expr pos0 (BoolLit False))))
  assertEqual "unary negation type" (Right (IntType pos0 Signed I32)) $ checkExprType (Expr pos0 (UnaryExpr NegOp (Expr pos0 (IntLit 1))))
  assertEqual "unary not type" (Right (BoolType pos0)) $ checkExprType (Expr pos0 (UnaryExpr NotOp (Expr pos0 (BoolLit False))))
  assertLeft "unary negation bool unsupported" $ checkExprType (Expr pos0 (UnaryExpr NegOp (Expr pos0 (BoolLit True))))
  assertLeft "unary not int unsupported" $ checkExprType (Expr pos0 (UnaryExpr NotOp (Expr pos0 (IntLit 1))))
  assertLeft "unary ampersand unsupported" $ checkExprType (Expr pos0 (UnaryExpr AmpersandOp (Expr pos0 (IntLit 1))))
  assertLeft "ordered bool comparison unsupported" $ checkExprType (Expr pos0 (BinaryExpr LtOp (Expr pos0 (BoolLit True)) (Expr pos0 (BoolLit False))))
  assertEqual "if expression type" (Right (IntType pos0 Signed I32)) $ checkExprType (Expr pos0 (IfExpr (Expr pos0 (BoolLit True)) (Block [] (Just (Expr pos0 (IntLit 1)))) (Just (Block [] (Just (Expr pos0 (IntLit 2)))))))
  assertEqual "function expression type" (Right (FnType [intType] intType)) $ checkExprType fnIdExpr
  assertLeft "calling non-function unsupported" $ checkExprType (Expr pos0 (CallExpr (Expr pos0 (IntLit 1)) []))
  assertLeft "function call arity mismatch" $ checkExprType (Expr pos0 (CallExpr fnIdExpr []))
  assertLeft "function return type mismatch" $ checkExprType (Expr pos0 (FnExpr [Param (identFrom "x") intType] (BoolType pos0) (Block [] (Just (Expr pos0 (VarExpr (identFrom "x")))))))
  assertLeft "if without else cannot produce int" $ checkProgram (Program [TopLevelStmt (Stmt pos0 (DeclStmt (ValueDecl Mutable (identFrom "x") (Just (IntType pos0 Signed I32)) (Just (Expr pos0 (IfExpr (Expr pos0 (BoolLit True)) (Block [] (Just (Expr pos0 (IntLit 1)))) Nothing))))))])
  assertLeft "missing annotation" $ checkProgram (Program [TopLevelStmt (Stmt pos0 (DeclStmt (ValueDecl Mutable (identFrom "x") Nothing Nothing)))])
  assertEqual "repl expression type checks" (Right (ReplExpr (Expr (IntType pos0 Signed I32) (IntLit 1)))) $ checkReplInput (ReplExpr (Expr pos0 (IntLit 1)))
  assertLeft "repl remembers unit variable type" $ do
    (_, env) <- checkReplInputWithEnv Map.empty replUnitDecl
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

assertEqual :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEqual name expected actual =
  if expected == actual
    then return ()
    else error $ name ++ " failed:\n  expected: " ++ show expected ++ "\n  actual:   " ++ show actual

assertLeft :: String -> Either e a -> IO ()
assertLeft _ (Left _) = return ()
assertLeft name (Right _) = error $ name ++ ": expected type error"

pos0 :: AlexPosn
pos0 = AlexPn 0 1 1

identFrom :: String -> Ident
identFrom = Ident pos0
