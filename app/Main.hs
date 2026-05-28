module Main where
import System.Environment
-- import qualified Parser (someFunc)
import Lexer (tokenize, token_posn)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then print "No file provided"
  else do
    let filename = head args
    content <- readFile filename
    mapM_ print (tokenize content) 