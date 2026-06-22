module ParserTest (testParser) where

import Parser.ErrorTest (testParseErrors)
import Parser.ProgramTest (testParseProgram)
import Parser.ReplTest (testParseRepl)

testParser :: IO ()
testParser = do
  testParseProgram
  testParseRepl
  testParseErrors
