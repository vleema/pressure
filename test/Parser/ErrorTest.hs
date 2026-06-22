module Parser.ErrorTest (testParseErrors) where

import Lexer (runAlex)
import Parser (parseProgram)
import TestUtil (assertLeft, assertRight)

testParseErrors :: IO ()
testParseErrors = do
  assertLeft "program requires semicolon" $ runAlex "1 + 2" parseProgram
  assertLeft "malformed expression" $ runAlex "x: int = 1 + ;" parseProgram
  assertLeft "chained comparisons are forbidden" $ runAlex "1 < 2 < 3;" parseProgram
  _ <- assertRight "if expression without else parses" $ runAlex "x: int = if true { 1 };" parseProgram
  return ()
