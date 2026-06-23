module Parser.ProgramTest (parserProgramTests) where

import Ast
import Lexer (runAlex)
import Parser (parseRepl)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestUtil (assertRight)

parserProgramTests :: TestTree
parserProgramTests = testGroup "program" [testCase "parses programs" testParseProgram]

parse :: String -> String -> IO ParsedRepl
parse name source = assertRight name $ runAlex source parseRepl

singleDecl :: ParsedRepl -> Maybe ParsedDecl
singleDecl = \case
  Repl [ReplStmt (ParsedStmt _ (ParsedDeclStmt decl))] -> Just decl
  _ -> Nothing

singleExpr :: ParsedRepl -> Maybe ParsedExpr
singleExpr = \case
  Repl [ReplStmt (ParsedStmt _ (ParsedExprStmt expr))] -> Just expr
  Repl [ReplExpr expr] -> Just expr
  _ -> Nothing

isIntSyntax :: TypeSyntax -> Bool
isIntSyntax (TypeSyntax _ (IntSyntax _ _)) = True
isIntSyntax _ = False

isIntLit :: Integer -> ParsedExpr -> Bool
isIntLit expected = \case
  ParsedExpr _ (ParsedIntLit actual) -> expected == actual
  _ -> False

isBinary :: BinaryOp -> (ParsedExpr -> Bool) -> (ParsedExpr -> Bool) -> ParsedExpr -> Bool
isBinary expectedOp left right = \case
  ParsedExpr _ (ParsedBinaryExpr actualOp l r) -> expectedOp == actualOp && left l && right r
  _ -> False

expect :: String -> Bool -> ParsedRepl -> IO ()
expect _ True _ = return ()
expect name False ast = error $ "unexpected AST for " ++ name ++ ": " ++ show ast

testParseProgram :: IO ()
testParseProgram = do
  ast <- parse "parse mutable declaration" "x: i32 = 42;"
  expect "mutable declaration" (case singleDecl ast of Just (ParsedValueDecl Mutable _ (Just typ) (Just (ParsedExpr _ (ParsedIntLit _)))) -> isIntSyntax typ; _ -> False) ast

  ast2 <- parse "parse constant declaration" "y: i32 : 7;"
  expect "constant declaration" (case singleDecl ast2 of Just (ParsedValueDecl Constant _ (Just typ) (Just (ParsedExpr _ (ParsedIntLit _)))) -> isIntSyntax typ; _ -> False) ast2

  ast4 <- parse "parse addition expression" "sum: i32 = 1 + 2 + 3;"
  expect "addition expression" (case singleDecl ast4 of Just (ParsedValueDecl Mutable _ (Just typ) (Just (ParsedExpr _ (ParsedBinaryExpr AddOp (ParsedExpr _ (ParsedBinaryExpr AddOp (ParsedExpr _ (ParsedIntLit 1)) (ParsedExpr _ (ParsedIntLit 2)))) (ParsedExpr _ (ParsedIntLit 3)))))) -> isIntSyntax typ; _ -> False) ast4

  ast5 <- parse "parse multiplication precedence" "value: i32 = 1 + 2 * 3;"
  expect "multiplication precedence" (case singleDecl ast5 of Just (ParsedValueDecl Mutable _ (Just typ) (Just (ParsedExpr _ (ParsedBinaryExpr AddOp (ParsedExpr _ (ParsedIntLit 1)) (ParsedExpr _ (ParsedBinaryExpr MulOp (ParsedExpr _ (ParsedIntLit 2)) (ParsedExpr _ (ParsedIntLit 3)))))))) -> isIntSyntax typ; _ -> False) ast5

  ast6 <- parse "parse division expression" "value: i32 = 8 / 4 / 2;"
  expect "division expression" (case singleDecl ast6 of Just (ParsedValueDecl Mutable _ (Just typ) (Just (ParsedExpr _ (ParsedBinaryExpr DivOp (ParsedExpr _ (ParsedBinaryExpr DivOp (ParsedExpr _ (ParsedIntLit 8)) (ParsedExpr _ (ParsedIntLit 4)))) (ParsedExpr _ (ParsedIntLit 2)))))) -> isIntSyntax typ; _ -> False) ast6

  ast7 <- parse "parse parenthesized expression" "value: i32 = (1 + 2) * 3;"
  expect "parenthesized expression" (case singleDecl ast7 of Just (ParsedValueDecl Mutable _ (Just typ) (Just (ParsedExpr _ (ParsedBinaryExpr MulOp (ParsedExpr _ (ParsedBinaryExpr AddOp (ParsedExpr _ (ParsedIntLit 1)) (ParsedExpr _ (ParsedIntLit 2)))) (ParsedExpr _ (ParsedIntLit 3)))))) -> isIntSyntax typ; _ -> False) ast7

  ast8 <- parse "parse bare expression" "1 + 2;"
  expect "bare expression" (case singleExpr ast8 of Just (ParsedExpr _ (ParsedBinaryExpr AddOp (ParsedExpr _ (ParsedIntLit 1)) (ParsedExpr _ (ParsedIntLit 2)))) -> True; _ -> False) ast8

  ast9 <- parse "parse variable reference as expression" "x;"
  expect "variable reference" (case singleExpr ast9 of Just (ParsedExpr _ (ParsedVarExpr (Ident _ "x"))) -> True; _ -> False) ast9

  ast10 <- parse "parse subtraction precedence" "value: i32 = 1 - 2 * 3;"
  expect "subtraction precedence" (case singleDecl ast10 of Just (ParsedValueDecl Mutable _ (Just typ) (Just (ParsedExpr _ (ParsedBinaryExpr SubOp (ParsedExpr _ (ParsedIntLit 1)) (ParsedExpr _ (ParsedBinaryExpr MulOp (ParsedExpr _ (ParsedIntLit 2)) (ParsedExpr _ (ParsedIntLit 3)))))))) -> isIntSyntax typ; _ -> False) ast10

  ast11 <- parse "parse boolean precedence" "true or false and 1 < 2;"
  expect "boolean precedence" (case singleExpr ast11 of Just (ParsedExpr _ (ParsedBinaryExpr OrOp _ _)) -> True; _ -> False) ast11

  ast12 <- parse "parse comparison after arithmetic" "1 + 2 * 3 == 7;"
  expect "comparison precedence" (case singleExpr ast12 of Just e -> isBinary EqOp (isBinary AddOp (isIntLit 1) (isBinary MulOp (isIntLit 2) (isIntLit 3))) (isIntLit 7) e; _ -> False) ast12

  ast13 <- parse "parse if expression with else" "x: int = if true { 1 } else { 2 };"
  expect "if expression" (case singleDecl ast13 of Just (ParsedValueDecl Mutable _ (Just typ) (Just (ParsedExpr _ (ParsedIfExpr _ _ (Just _))))) -> isIntSyntax typ; _ -> False) ast13

  ast14 <- parse "parse if statement without else" "if true { x: int = 1; }"
  expect "if statement" (case singleExpr ast14 of Just (ParsedExpr _ (ParsedIfExpr _ _ Nothing)) -> True; _ -> False) ast14

  ast15 <- parse "parse unary negation" "-1;"
  expect "unary negation" (case singleExpr ast15 of Just (ParsedExpr _ (ParsedUnaryExpr NegOp (ParsedExpr _ (ParsedIntLit 1)))) -> True; _ -> False) ast15

  ast16 <- parse "parse unary not" "!false;"
  expect "unary not" (case singleExpr ast16 of Just (ParsedExpr _ (ParsedUnaryExpr NotOp (ParsedExpr _ (ParsedBoolLit False)))) -> True; _ -> False) ast16

  ast17 <- parse "parse unary ampersand" "&x;"
  expect "unary ampersand" (case singleExpr ast17 of Just (ParsedExpr _ (ParsedUnaryExpr AmpersandOp (ParsedExpr _ (ParsedVarExpr (Ident _ "x"))))) -> True; _ -> False) ast17

  ast18 <- parse "parse unary precedence" "1 * -2;"
  expect "unary precedence" (case singleExpr ast18 of Just (ParsedExpr _ (ParsedBinaryExpr MulOp (ParsedExpr _ (ParsedIntLit 1)) (ParsedExpr _ (ParsedUnaryExpr NegOp (ParsedExpr _ (ParsedIntLit 2)))))) -> True; _ -> False) ast18

  ast19 <- parse "parse function expression" "add :: fn(a: i32, b: i32) -> i32 { a + b };"
  expect "function expression" (case singleDecl ast19 of Just (ParsedValueDecl Constant _ Nothing (Just (ParsedExpr _ (ParsedFnExpr [Param _ p1, Param _ p2] ret _)))) -> isIntSyntax p1 && isIntSyntax p2 && isIntSyntax ret; _ -> False) ast19

  ast20 <- parse "parse call expression" "add(1, 2);"
  expect "call expression" (case singleExpr ast20 of Just (ParsedExpr _ (ParsedCallExpr (ParsedExpr _ (ParsedVarExpr (Ident _ "add"))) [ParsedExpr _ (ParsedIntLit 1), ParsedExpr _ (ParsedIntLit 2)])) -> True; _ -> False) ast20
