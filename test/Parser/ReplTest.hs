module Parser.ReplTest (testParseRepl) where

import Ast
import Lexer (runAlex)
import Parser (parseRepl)
import TestUtil (assertRight)

testParseRepl :: IO ()
testParseRepl = do
  ast <-
    assertRight "repl: declaration without semicolon" $
      runAlex "x: int = 42" parseRepl
  case ast of
    ReplStmt (Stmt _ (DeclStmt (ValueDecl Mutable _ (Just (IntType _ _ _)) (Just (Expr _ (IntLit 42)))))) -> return ()
    other -> error $ "unexpected AST for repl declaration: " ++ show other

  ast2 <-
    assertRight "repl: declaration with semicolon" $
      runAlex "x: int = 42;" parseRepl
  case ast2 of
    ReplStmt (Stmt _ (DeclStmt (ValueDecl Mutable _ (Just (IntType _ _ _)) (Just (Expr _ (IntLit 42)))))) -> return ()
    other -> error $ "unexpected AST for repl declaration stmt: " ++ show other

  ast3 <-
    assertRight "repl: bare expression" $
      runAlex "1 + 2" parseRepl
  case ast3 of
    ReplExpr (Expr _ (BinaryExpr AddOp (Expr _ (IntLit 1)) (Expr _ (IntLit 2)))) -> return ()
    other -> error $ "unexpected AST for repl expression: " ++ show other

  ast4 <-
    assertRight "repl: expression with semicolon" $
      runAlex "1 + 2;" parseRepl
  case ast4 of
    ReplStmt (Stmt _ (ExprStmt (Expr _ (BinaryExpr AddOp (Expr _ (IntLit 1)) (Expr _ (IntLit 2)))))) -> return ()
    other -> error $ "unexpected AST for repl expression stmt: " ++ show other

  ast5 <-
    assertRight "repl: variable reference" $
      runAlex "x" parseRepl
  case ast5 of
    ReplExpr (Expr _ (VarExpr (Ident _ "x"))) -> return ()
    other -> error $ "unexpected AST for repl variable reference: " ++ show other

  ast6 <-
    assertRight "repl: variable in expression" $
      runAlex "x + 1" parseRepl
  case ast6 of
    ReplExpr (Expr _ (BinaryExpr AddOp (Expr _ (VarExpr (Ident _ "x"))) (Expr _ (IntLit 1)))) -> return ()
    other -> error $ "unexpected AST for repl variable in expr: " ++ show other
