module Lexer.LexerTest (testLexer) where

import Lexer (AlexPosn (..), Token (..), tokenizeEither)
import TestUtil

testLexer :: IO ()
testLexer = do
  testTokenizeKeywords
  testTokenizeOperators
  testTokenizeDelimiters
  testTokenizeIdentifiers
  testTokenizeNumbers
  testTokenizeDeclaration
  testTokenizeEqualityOperator
  testTokenizeComment
  testTokenizePositions
  testTokenizeInvalidCharacter

testTokenizeKeywords :: IO ()
testTokenizeKeywords = do
  tokens <-
    assertRight "tokenize keywords" $
      tokenizeEither "if else true false for continue break fn struct enum return"
  assertEqual "keyword count" 11 (length tokens)

testTokenizeOperators :: IO ()
testTokenizeOperators = do
  ops <-
    assertRight "tokenize operators" $
      tokenizeEither "= < > == != <= >= -> and or ! + - >> << * / &"
  assertEqual "operator count" 18 (length ops)

testTokenizeDelimiters :: IO ()
testTokenizeDelimiters = do
  parens <-
    assertRight "tokenize delimiters" $
      tokenizeEither "( ) { } [ ] . , ; : ' \""
  assertEqual "delimiter count" 12 (length parens)

testTokenizeIdentifiers :: IO ()
testTokenizeIdentifiers = do
  ids <- assertRight "tokenize identifiers" $ tokenizeEither "foo bar_baz x'"
  assertEqual "identifier count" 3 (length ids)

testTokenizeNumbers :: IO ()
testTokenizeNumbers = do
  nums <- assertRight "tokenize numbers" $ tokenizeEither "42 3.14"
  assertEqual "number count" 2 (length nums)

testTokenizeDeclaration :: IO ()
testTokenizeDeclaration = do
  let input = "let x: i32 = 42;"
  _ <- assertRight "tokenize declaration" $ tokenizeEither input
  return ()

testTokenizeEqualityOperator :: IO ()
testTokenizeEqualityOperator = do
  eq <- assertRight "tokenize equality operator" $ tokenizeEither "=="
  case eq of
    [CmpEq _] -> return ()
    other -> error $ "expected CmpEq, got " ++ show other

testTokenizeComment :: IO ()
testTokenizeComment = do
  comment <- assertRight "skip line comment" $ tokenizeEither "x // ignored\ny"
  case comment of
    [Id _ "x", Id _ "y"] -> return ()
    other -> error $ "expected identifiers around skipped comment, got " ++ show other

testTokenizePositions :: IO ()
testTokenizePositions = do
  positioned <- assertRight "token positions" $ tokenizeEither "x\n  y"
  case positioned of
    [Id (AlexPn _ 1 1) "x", Id (AlexPn _ 2 3) "y"] -> return ()
    other -> error $ "unexpected token positions: " ++ show other

testTokenizeInvalidCharacter :: IO ()
testTokenizeInvalidCharacter = do
  assertLeft "invalid character" $ tokenizeEither "@"
  return ()
