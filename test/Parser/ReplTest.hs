module Parser.ReplTest (parserReplTests) where

import Ast
import Lexer (runAlex)
import Parser (parseRepl)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestUtil (assertRight)

parserReplTests :: TestTree
parserReplTests = testGroup "repl" [testCase "parses repl input" testParseRepl]

testParseRepl :: IO ()
testParseRepl = do
  ast <-
    assertRight "repl: declaration without semicolon" $
      runAlex "x: int = 42" parseRepl
  case ast of
    Repl [ReplStmt (ParsedStmt _ (ParsedDeclStmt (ParsedValueDecl Mutable _ (Just (TypeSyntax _ (IntSyntax _ _))) (Just (ParsedExpr _ (ParsedIntLit 42))))))] -> return ()
    other -> error $ "unexpected AST for repl declaration: " ++ show other

  ast2 <-
    assertRight "repl: declaration with semicolon" $
      runAlex "x: int = 42;" parseRepl
  case ast2 of
    Repl [ReplStmt (ParsedStmt _ (ParsedDeclStmt (ParsedValueDecl Mutable _ (Just (TypeSyntax _ (IntSyntax _ _))) (Just (ParsedExpr _ (ParsedIntLit 42))))))] -> return ()
    other -> error $ "unexpected AST for repl declaration stmt: " ++ show other

  ast3 <-
    assertRight "repl: bare expression" $
      runAlex "1 + 2" parseRepl
  case ast3 of
    Repl [ReplExpr (ParsedExpr _ (ParsedBinaryExpr AddOp (ParsedExpr _ (ParsedIntLit 1)) (ParsedExpr _ (ParsedIntLit 2))))] -> return ()
    other -> error $ "unexpected AST for repl expression: " ++ show other

  ast4 <-
    assertRight "repl: expression with semicolon" $
      runAlex "1 + 2;" parseRepl
  case ast4 of
    Repl [ReplExpr (ParsedExpr _ (ParsedBinaryExpr AddOp (ParsedExpr _ (ParsedIntLit 1)) (ParsedExpr _ (ParsedIntLit 2))))] -> return ()
    other -> error $ "unexpected AST for repl expression stmt: " ++ show other

  ast5 <-
    assertRight "repl: variable reference" $
      runAlex "x" parseRepl
  case ast5 of
    Repl [ReplExpr (ParsedExpr _ (ParsedVarExpr (Ident _ "x")))] -> return ()
    other -> error $ "unexpected AST for repl variable reference: " ++ show other

  ast6 <-
    assertRight "repl: variable in expression" $
      runAlex "x + 1" parseRepl
  case ast6 of
    Repl [ReplExpr (ParsedExpr _ (ParsedBinaryExpr AddOp (ParsedExpr _ (ParsedVarExpr (Ident _ "x"))) (ParsedExpr _ (ParsedIntLit 1))))] -> return ()
    other -> error $ "unexpected AST for repl variable in expr: " ++ show other
