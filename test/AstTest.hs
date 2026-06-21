module AstTest (testAst) where

import Ast hiding (Error)
import Control.Monad.Except (runExcept)
import Control.Monad.State (runStateT)
import Data.Map.Strict qualified as Map
import Eval (Env, Error (..), evalExpr, evalProgram)
import Lexer (runAlex)
import Lexer qualified
import Parser (parseProgram)

assertRight :: String -> Either String a -> IO a
assertRight name (Left err) = error $ name ++ " failed with: " ++ err
assertRight _ (Right x) = return x

assertExpr :: String -> Expr Type -> Env -> Value -> IO ()
assertExpr name expr env expected = do
  case runExcept (runStateT (evalExpr expr) env) of
    Left err -> error $ name ++ " failed: " ++ show err
    Right (val, _) ->
      if val == expected
        then return ()
        else error $ name ++ ": expected " ++ show expected ++ " but got " ++ show val

assertEvalError :: String -> Expr Type -> Env -> Error -> IO ()
assertEvalError name expr env expectedErr = do
  case runExcept (runStateT (evalExpr expr) env) of
    Left err ->
      if err == expectedErr
        then return ()
        else error $ name ++ ": expected error '" ++ show expectedErr ++ "' but got '" ++ show err ++ "'"
    Right (val, _) -> error $ name ++ ": expected error but got " ++ show val

testAst :: IO ()
testAst = do
  testIntLit
  testFloatLit
  testBoolLit
  testIntAdd
  testFloatAdd
  testIntDiv
  testFloatDiv
  testDivByZero
  testTypeNameAnnotation
  testBoolInArithmeticError
  testBoolInArithmeticRightError
  testTypeMismatchError
  testFloatNarrowingError
  testVarDeclAndLookup
  testVarUndefined
  testVarDefaultValue
  testIntMul
  testFloatMul
  testIntSub
  testFloatSub
  testMixedSubEval
  testBoolDefaultValue
  testMissingAnnotationError
  testIfExpressionEval
  testIfStatementEval
  testIfElseStatementEval
  testUnaryNegEval
  testUnaryNotEval
  testFunctionEval
  testClosureCapturesByValue
  testFunctionLocalScope

withTokens :: String -> String -> (ParsedProgram -> IO ()) -> IO ()
withTokens name source f = do
  ast <- assertRight ("parse " ++ name) $ runAlex source parseProgram
  f ast

checkOk :: String -> String -> IO ()
checkOk name source =
  withTokens name source $ \ast ->
    case checkProgram ast of
      Right () -> return ()
      Left err -> error $ name ++ " failed: " ++ show err

checkErr :: String -> String -> IO ()
checkErr name source =
  withTokens name source $ \ast ->
    case checkProgram ast of
      Left _ -> return ()
      Right () -> error $ name ++ ": expected type error but passed"

evalParsed :: String -> ParsedProgram -> IO (Either Error (Value, Env))
evalParsed name ast =
  case checkProgramTyped ast of
    Left err -> error $ name ++ " type check failed: " ++ show err
    Right typedAst -> return $ runExcept (runStateT (evalProgram typedAst) Map.empty)

testIntLit :: IO ()
testIntLit = do
  checkOk "int literal" "x: int = 42;"
  withTokens "int literal eval" "x: int = 42;" $ \ast -> do
    result <- evalParsed "int literal eval" ast
    case result of
      Left err -> error $ "int literal eval failed: " ++ show err
      Right (val, env) -> do
        if val == VUnit then return () else error $ "expected VUnit got " ++ show val
        case Map.lookup "x" env of
          Just (VInt Signed I32 42) -> return ()
          other -> error $ "expected x = 42, got " ++ show other

testFloatLit :: IO ()
testFloatLit = do
  checkOk "float literal" "x: float = 3.14;"
  withTokens "float literal eval" "x: float = 3.14;" $ \ast -> do
    result <- evalParsed "float literal eval" ast
    case result of
      Left err -> error $ "float literal eval failed: " ++ show err
      Right (_, env) ->
        case Map.lookup "x" env of
          Just (VFloat F64 3.14) -> return ()
          other -> error $ "expected 3.14, got " ++ show other

testBoolLit :: IO ()
testBoolLit = do
  checkOk "bool literal" "x: bool = true;"
  withTokens "bool literal eval" "x: bool = true;" $ \ast -> do
    result <- evalParsed "bool literal eval" ast
    case result of
      Left err -> error $ "bool literal eval failed: " ++ show err
      Right (_, env) ->
        case Map.lookup "x" env of
          Just (VBool True) -> return ()
          other -> error $ "expected true, got " ++ show other

testIntAdd :: IO ()
testIntAdd = checkOk "int addition" "x: int = 1 + 2;"

testFloatAdd :: IO ()
testFloatAdd = checkOk "float addition" "x: float = 1.0 + 2.0;"

testIntDiv :: IO ()
testIntDiv = checkOk "int division" "x: int = 8 / 4;"

testFloatDiv :: IO ()
testFloatDiv = checkOk "float division" "x: float = 3.0 / 2.0;"

testDivByZero :: IO ()
testDivByZero = do
  withTokens "division by zero int" "x: int = 1 / 0;" $ \ast -> do
    result <- evalParsed "division by zero int" ast
    case result of
      Left (RuntimeError "division by zero") -> return ()
      Left err -> error $ "expected 'division by zero' got '" ++ show err ++ "'"
      Right _ -> error "expected runtime error for division by zero"

  withTokens "division by zero float" "x: float = 1.0 / 0.0;" $ \ast -> do
    result <- evalParsed "division by zero float" ast
    case result of
      Left (RuntimeError "division by zero") -> return ()
      Left err -> error $ "expected 'division by zero' got '" ++ show err ++ "'"
      Right _ -> error "expected runtime error for division by zero"

testTypeNameAnnotation :: IO ()
testTypeNameAnnotation = checkOk "TypeName annotation" "x: i32 = 42;"

testBoolInArithmeticError :: IO ()
testBoolInArithmeticError = checkErr "bool in arithmetic" "x: int = true + 1;"

testBoolInArithmeticRightError :: IO ()
testBoolInArithmeticRightError = checkErr "bool on right of arithmetic" "x: int = 1 + true;"

testTypeMismatchError :: IO ()
testTypeMismatchError = checkErr "type mismatch" "x: bool = 42;"

testFloatNarrowingError :: IO ()
testFloatNarrowingError = checkErr "float to int narrowing" "x: int = 3.14;"

testVarDeclAndLookup :: IO ()
testVarDeclAndLookup = do
  let decl42 = "x: int = 42;"
  withTokens "parse decl42" decl42 $ \ast -> do
    result <- evalParsed "var decl and lookup" ast
    case result of
      Right (_, env) ->
        assertExpr "x after decl" (Expr UnitType (VarExpr (identFrom "x"))) env (VInt Signed I32 42)
      Left err -> error $ "eval failed: " ++ show err

testVarUndefined :: IO ()
testVarUndefined =
  assertEvalError "undefined variable" (Expr UnitType (VarExpr (identFrom "z"))) Map.empty (RuntimeError "undefined variable: z")

testVarDefaultValue :: IO ()
testVarDefaultValue = do
  withTokens "decl without init" "x: int;" $ \ast -> do
    result <- evalParsed "decl without init" ast
    case result of
      Right (_, env) ->
        case Map.lookup "x" env of
          Just (VInt Signed I32 0) -> return ()
          other -> error $ "expected x = 0 for uninitialized int, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testIntMul :: IO ()
testIntMul = checkOk "int multiplication" "x: int = 3 * 4;"

testFloatMul :: IO ()
testFloatMul = checkOk "float multiplication" "x: float = 1.5 * 2.0;"

testIntSub :: IO ()
testIntSub = checkOk "int subtraction" "x: int = 8 - 3;"

testFloatSub :: IO ()
testFloatSub = checkOk "float subtraction" "x: float = 8.5 - 3.0;"

testMixedSubEval :: IO ()
testMixedSubEval =
  assertExpr
    "float subtraction eval"
    (Expr UnitType (BinaryExpr SubOp (Expr UnitType (FloatLit 8.5)) (Expr UnitType (FloatLit 3.0))))
    Map.empty
    (VFloat F64 5.5)

testBoolDefaultValue :: IO ()
testBoolDefaultValue = do
  withTokens "bool decl without init" "x: bool;" $ \ast -> do
    result <- evalParsed "bool decl without init" ast
    case result of
      Right (_, env) ->
        case Map.lookup "x" env of
          Just (VBool False) -> return ()
          other -> error $ "expected x = false for uninitialized bool, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testMissingAnnotationError :: IO ()
testMissingAnnotationError =
  case checkProgram (Program [TopLevelStmt (Stmt pos0 (DeclStmt (ValueDecl Mutable (identFrom "x") Nothing Nothing)))]) of
    Left _ -> return ()
    Right () -> error "missing annotation: expected type error but passed"

testIfExpressionEval :: IO ()
testIfExpressionEval = do
  withTokens "if expression eval" "x: int = if true { 1 } else { 2 };" $ \ast -> do
    result <- evalParsed "if expression eval" ast
    case result of
      Right (_, env) ->
        case Map.lookup "x" env of
          Just (VInt Signed I32 1) -> return ()
          other -> error $ "expected x = 1, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testIfStatementEval :: IO ()
testIfStatementEval = do
  withTokens "if statement eval" "if false { x: int = 1; }" $ \ast -> do
    result <- evalParsed "if statement eval" ast
    case result of
      Right (_, env) ->
        case Map.lookup "x" env of
          Nothing -> return ()
          other -> error $ "expected x to be absent, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testIfElseStatementEval :: IO ()
testIfElseStatementEval = do
  withTokens "if else statement eval" "if false { x: int = 1; } else { y: float = 42.0; }" $ \ast -> do
    result <- evalParsed "if else statement eval" ast
    case result of
      Right (_, env) ->
        case Map.lookup "x" env of
          Nothing -> return ()
          other -> error $ "expected x to be absent, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testUnaryNegEval :: IO ()
testUnaryNegEval = do
  withTokens "unary negation eval" "x: int = -42;" $ \ast -> do
    result <- evalParsed "unary negation eval" ast
    case result of
      Right (_, env) ->
        case Map.lookup "x" env of
          Just (VInt Signed I32 (-42)) -> return ()
          other -> error $ "expected x = -42, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testUnaryNotEval :: IO ()
testUnaryNotEval = do
  withTokens "unary not eval" "x: bool = !false;" $ \ast -> do
    result <- evalParsed "unary not eval" ast
    case result of
      Right (_, env) ->
        case Map.lookup "x" env of
          Just (VBool True) -> return ()
          other -> error $ "expected x = true, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testFunctionEval :: IO ()
testFunctionEval = do
  withTokens "function eval" "add :: fn(a: i32, b: i32) -> i32 { a + b }; result: i32 = add(1, 2);" $ \ast -> do
    result <- evalParsed "function eval" ast
    case result of
      Right (_, env) ->
        case Map.lookup "result" env of
          Just (VInt Signed I32 3) -> return ()
          other -> error $ "expected result = 3, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testClosureCapturesByValue :: IO ()
testClosureCapturesByValue = do
  withTokens "closure captures by value" "x :: 10; addX :: fn(y: i32) -> i32 { x + y }; x :: 20; result: i32 = addX(5);" $ \ast -> do
    result <- evalParsed "closure captures by value" ast
    case result of
      Right (_, env) ->
        case Map.lookup "result" env of
          Just (VInt Signed I32 15) -> return ()
          other -> error $ "expected result = 15, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testFunctionLocalScope :: IO ()
testFunctionLocalScope = do
  withTokens "function local scope" "f :: fn() -> i32 { x :: 1; x }; result: i32 = f();" $ \ast -> do
    result <- evalParsed "function local scope" ast
    case result of
      Right (_, env) -> do
        case Map.lookup "result" env of
          Just (VInt Signed I32 1) -> return ()
          other -> error $ "expected result = 1, got " ++ show other
        case Map.lookup "x" env of
          Nothing -> return ()
          other -> error $ "expected function local x to be absent, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

pos0 :: Lexer.AlexPosn
pos0 = Lexer.AlexPn 0 1 1

identFrom :: String -> Ident
identFrom name = Ident pos0 name
