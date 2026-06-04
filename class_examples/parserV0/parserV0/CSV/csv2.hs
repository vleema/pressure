#!/usr/bin/env cabal
{- cabal:
build-depends: base, parsec
-}
import Text.ParserCombinators.Parsec

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

mainParser :: String -> IO ()
mainParser input = case parseCSV input of
                  { Left err -> print err
                  ; Right ans -> print ans
                  }

main :: IO ()
main = do
    str <- getContents
    case parseCSV str of
        Left err ->
            print err
        Right res ->
            mapM_ print res
