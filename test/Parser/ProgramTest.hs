module Parser.ProgramTest (testParseProgram) where

import Ast
import Lexer (runAlex)
import Parser (parseProgram)
import TestUtil (assertRight)

testParseProgram :: IO ()
testParseProgram = do
  ast <-
    assertRight "parse mutable declaration" $
      runAlex "x: i32 = 42;" parseProgram
  case ast of
    Program [TopLevelStmt (Stmt _ (DeclStmt (ValueDecl Mutable _ (Just (IntType _ _ _)) (Just (Expr _ (IntLit _))))))] -> return ()
    other -> error $ "unexpected AST for mutable declaration: " ++ show other

  ast2 <-
    assertRight "parse constant declaration" $
      runAlex "y: i32 : 7;" parseProgram
  case ast2 of
    Program [TopLevelStmt (Stmt _ (DeclStmt (ValueDecl Constant _ (Just (IntType _ _ _)) (Just (Expr _ (IntLit _))))))] -> return ()
    other -> error $ "unexpected AST for constant declaration: " ++ show other

  ast4 <-
    assertRight "parse addition expression" $
      runAlex "sum: i32 = 1 + 2 + 3;" parseProgram
  case ast4 of
    Program [TopLevelStmt (Stmt _ (DeclStmt (ValueDecl Mutable _ (Just (IntType _ _ _)) (Just (Expr _ (BinaryExpr AddOp (Expr _ (BinaryExpr AddOp (Expr _ (IntLit 1)) (Expr _ (IntLit 2)))) (Expr _ (IntLit 3))))))))] -> return ()
    other -> error $ "unexpected AST for addition expression: " ++ show other

  ast5 <-
    assertRight "parse multiplication precedence" $
      runAlex "value: i32 = 1 + 2 * 3;" parseProgram
  case ast5 of
    Program [TopLevelStmt (Stmt _ (DeclStmt (ValueDecl Mutable _ (Just (IntType _ _ _)) (Just (Expr _ (BinaryExpr AddOp (Expr _ (IntLit 1)) (Expr _ (BinaryExpr MulOp (Expr _ (IntLit 2)) (Expr _ (IntLit 3))))))))))] -> return ()
    other -> error $ "unexpected AST for multiplication precedence: " ++ show other

  ast6 <-
    assertRight "parse division expression" $
      runAlex "value: i32 = 8 / 4 / 2;" parseProgram
  case ast6 of
    Program [TopLevelStmt (Stmt _ (DeclStmt (ValueDecl Mutable _ (Just (IntType _ _ _)) (Just (Expr _ (BinaryExpr DivOp (Expr _ (BinaryExpr DivOp (Expr _ (IntLit 8)) (Expr _ (IntLit 4)))) (Expr _ (IntLit 2))))))))] -> return ()
    other -> error $ "unexpected AST for division expression: " ++ show other

  ast7 <-
    assertRight "parse parenthesized expression" $
      runAlex "value: i32 = (1 + 2) * 3;" parseProgram
  case ast7 of
    Program [TopLevelStmt (Stmt _ (DeclStmt (ValueDecl Mutable _ (Just (IntType _ _ _)) (Just (Expr _ (BinaryExpr MulOp (Expr _ (BinaryExpr AddOp (Expr _ (IntLit 1)) (Expr _ (IntLit 2)))) (Expr _ (IntLit 3))))))))] -> return ()
    other -> error $ "unexpected AST for parenthesized expression: " ++ show other

  ast8 <-
    assertRight "parse bare expression as program" $
      runAlex "1 + 2;" parseProgram
  case ast8 of
    Program [TopLevelStmt (Stmt _ (ExprStmt (Expr _ (BinaryExpr AddOp (Expr _ (IntLit 1)) (Expr _ (IntLit 2))))))] -> return ()
    other -> error $ "unexpected AST for bare expression: " ++ show other

  ast9 <-
    assertRight "parse variable reference as expression" $
      runAlex "x;" parseProgram
  case ast9 of
    Program [TopLevelStmt (Stmt _ (ExprStmt (Expr _ (VarExpr (Ident _ "x")))))] -> return ()
    other -> error $ "unexpected AST for variable reference: " ++ show other

  ast10 <-
    assertRight "parse subtraction precedence" $
      runAlex "value: i32 = 1 - 2 * 3;" parseProgram
  case ast10 of
    Program [TopLevelStmt (Stmt _ (DeclStmt (ValueDecl Mutable _ (Just (IntType _ _ _)) (Just (Expr _ (BinaryExpr SubOp (Expr _ (IntLit 1)) (Expr _ (BinaryExpr MulOp (Expr _ (IntLit 2)) (Expr _ (IntLit 3))))))))))] -> return ()
    other -> error $ "unexpected AST for subtraction precedence: " ++ show other

  ast11 <-
    assertRight "parse boolean precedence" $
      runAlex "true or false and 1 < 2;" parseProgram
  case ast11 of
    Program [TopLevelStmt (Stmt _ (ExprStmt (Expr _ (BinaryExpr OrOp _ _))))] -> return ()
    other -> error $ "unexpected AST for boolean precedence: " ++ show other

  ast12 <-
    assertRight "parse comparison after arithmetic" $
      runAlex "1 + 2 * 3 == 7;" parseProgram
  case ast12 of
    Program [TopLevelStmt (Stmt _ (ExprStmt (Expr _ (BinaryExpr EqOp l r))))] ->
      case (l, r) of
        (Expr _ (BinaryExpr AddOp (Expr _ (IntLit 1)) (Expr _ (BinaryExpr MulOp (Expr _ (IntLit 2)) (Expr _ (IntLit 3))))), Expr _ (IntLit 7)) -> return ()
        _ -> error $ "unexpected AST for comparison precedence: " ++ show ast12
    other -> error $ "unexpected AST for comparison precedence: " ++ show other

  ast13 <-
    assertRight "parse if expression with else" $
      runAlex "x: int = if true { 1 } else { 2 };" parseProgram
  case ast13 of
    Program [TopLevelStmt (Stmt _ (DeclStmt (ValueDecl Mutable _ (Just (IntType _ _ _)) (Just (Expr _ (IfExpr _ _ (Just _)))))))] -> return ()
    other -> error $ "unexpected AST for if expression: " ++ show other

  ast14 <-
    assertRight "parse if statement without else" $
      runAlex "if true { x: int = 1; }" parseProgram
  case ast14 of
    Program [TopLevelStmt (Stmt _ (ExprStmt (Expr _ (IfExpr _ _ Nothing))))] -> return ()
    other -> error $ "unexpected AST for if statement: " ++ show other

  ast15 <-
    assertRight "parse unary negation" $
      runAlex "-1;" parseProgram
  case ast15 of
    Program [TopLevelStmt (Stmt _ (ExprStmt (Expr _ (UnaryExpr NegOp (Expr _ (IntLit 1))))))] -> return ()
    other -> error $ "unexpected AST for unary negation: " ++ show other

  ast16 <-
    assertRight "parse unary not" $
      runAlex "!false;" parseProgram
  case ast16 of
    Program [TopLevelStmt (Stmt _ (ExprStmt (Expr _ (UnaryExpr NotOp (Expr _ (BoolLit False))))))] -> return ()
    other -> error $ "unexpected AST for unary not: " ++ show other

  ast17 <-
    assertRight "parse unary ampersand" $
      runAlex "&x;" parseProgram
  case ast17 of
    Program [TopLevelStmt (Stmt _ (ExprStmt (Expr _ (UnaryExpr AmpersandOp (Expr _ (VarExpr (Ident _ "x")))))))] -> return ()
    other -> error $ "unexpected AST for unary ampersand: " ++ show other

  ast18 <-
    assertRight "parse unary precedence" $
      runAlex "1 * -2;" parseProgram
  case ast18 of
    Program [TopLevelStmt (Stmt _ (ExprStmt (Expr _ (BinaryExpr MulOp (Expr _ (IntLit 1)) (Expr _ (UnaryExpr NegOp (Expr _ (IntLit 2))))))))] -> return ()
    other -> error $ "unexpected AST for unary precedence: " ++ show other

  ast19 <-
    assertRight "parse function expression" $
      runAlex "add :: fn(a: i32, b: i32) -> i32 { a + b };" parseProgram
  case ast19 of
    Program [TopLevelStmt (Stmt _ (DeclStmt (ValueDecl Constant _ Nothing (Just (Expr _ (FnExpr [Param _ (IntType _ _ _), Param _ (IntType _ _ _)] (IntType _ _ _) _))))))] -> return ()
    other -> error $ "unexpected AST for function expression: " ++ show other

  ast20 <-
    assertRight "parse call expression" $
      runAlex "add(1, 2);" parseProgram
  case ast20 of
    Program [TopLevelStmt (Stmt _ (ExprStmt (Expr _ (CallExpr (Expr _ (VarExpr (Ident _ "add"))) [Expr _ (IntLit 1), Expr _ (IntLit 2)]))))] -> return ()
    other -> error $ "unexpected AST for call expression: " ++ show other
