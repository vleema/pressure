module Lang (repl, run) where

import Ast (ParsedRepl, Repl (..), Value)
import Ast.Typecheck qualified as Type
import Control.Monad (forever, when)
import Control.Monad.Except (ExceptT (..), catchError, liftEither, runExcept, runExceptT, throwError)
import Control.Monad.State (StateT, evalStateT, get, lift, liftIO, put, runStateT)
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isSpace)
import Eval (Env, Eval, evalReplInput)
import Eval qualified
import Parser (genAst, parseRepl)
import System.IO (hFlush, isEOF, stdout)

-- TODO: Separate REPL into separate module and add support for control characters.

type ReplState = (Env, Type.TypeEnv)

type REPL a = StateT ReplState (ExceptT Error IO) a

repl :: IO ()
repl = do
  _ <- runExceptT $ evalStateT replLoop ([], [])
  putStrLn "Goodbye!"

replLoop :: REPL ()
replLoop = forever $ replStep `catchError` handleError
  where
    handleError Exit = lift $ throwError Exit
    handleError err = liftIO $ putStrLn $ render err

run :: String -> IO ()
run input = do
  _ <- runExceptT $ evalStateT (eval input) ([], [])
  return ()

replStep :: REPL ()
replStep = do
  liftIO $ putStr ">> " >> hFlush stdout

  done <- liftIO isEOF
  when done $ lift $ throwError Exit

  line <- liftIO getLine
  when (trim line == ":q") $ lift $ throwError Exit

  (val, ast) <- eval line

  liftIO $ case ast of
    ReplExpr _ -> print val
    ReplStmt _ -> return ()

eval :: String -> REPL (Value, ParsedRepl)
eval input = do
  ast <- liftEither $ first ParseError $ genAst input parseRepl
  (_, typeEnv) <- get
  (typedAst, nextTypeEnv) <- liftEither $ first TypeError $ Type.checkReplInputWithEnv typeEnv ast
  val <- liftEval nextTypeEnv $ evalReplInput typedAst

  return (val, ast)

data Error
  = ParseError String
  | TypeError Type.Error
  | RuntimeError Eval.Error
  | Exit
  deriving (Show, Eq)

liftEval :: Type.TypeEnv -> Eval Value -> REPL Value
liftEval nextTypeEnv action = do
  (env, _) <- get
  case runExcept (runStateT action env) of
    Left err -> lift $ throwError $ RuntimeError err
    Right (val, nextEnv) -> do
      put (nextEnv, nextTypeEnv)
      return val

render :: Error -> String
render = \case
  ParseError e -> "parser error: " ++ e
  TypeError e -> "type error: " ++ show e
  RuntimeError e -> "runtime error: " ++ show e
  Exit -> ""

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
