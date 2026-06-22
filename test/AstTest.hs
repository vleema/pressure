module AstTest (testAst) where

import Ast hiding (Error)
import Control.Monad.Except (runExcept)
import Control.Monad.State (runStateT)
import Data.Map.Strict qualified as Map
import Eval (Env, Error (..), evalExpr, evalProgram, evalReplInput)
import Lexer (runAlex)
import Lexer qualified
import Parser (parseProgram, parseRepl)

emptyEnv :: Env
emptyEnv = []

lookupValue :: String -> Env -> Maybe Value
lookupValue _ [] = Nothing
lookupValue name (scope : rest) =
  case Map.lookup name scope of
    Just v -> Just v
    Nothing -> lookupValue name rest

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
  testDirectRecursion
  testTopLevelMutualRecursion
  testLocalMutualRecursionRejected
  testForwardFunctionReference
  testFunctionUsesGlobal
  testReplRecursiveFunction
  testDuplicateParamsRejected
  testDuplicateFunctionsRejected
  testDuplicateDeclarationsRejected
  testNestedFunctionCaptureRejected

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
    Right typedAst -> return $ runExcept (runStateT (evalProgram typedAst) emptyEnv)

testIntLit :: IO ()
testIntLit = do
  checkOk "int literal" "x: int = 42;"
  withTokens "int literal eval" "x: int = 42;" $ \ast -> do
    result <- evalParsed "int literal eval" ast
    case result of
      Left err -> error $ "int literal eval failed: " ++ show err
      Right (val, env) -> do
        if val == VUnit then return () else error $ "expected VUnit got " ++ show val
        case lookupValue "x" env of
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
        case lookupValue "x" env of
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
        case lookupValue "x" env of
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
  assertEvalError "undefined variable" (Expr UnitType (VarExpr (identFrom "z"))) emptyEnv (RuntimeError "undefined variable: z")

testVarDefaultValue :: IO ()
testVarDefaultValue = do
  withTokens "decl without init" "x: int;" $ \ast -> do
    result <- evalParsed "decl without init" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
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
    emptyEnv
    (VFloat F64 5.5)

testBoolDefaultValue :: IO ()
testBoolDefaultValue = do
  withTokens "bool decl without init" "x: bool;" $ \ast -> do
    result <- evalParsed "bool decl without init" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
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
        case lookupValue "x" env of
          Just (VInt Signed I32 1) -> return ()
          other -> error $ "expected x = 1, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testIfStatementEval :: IO ()
testIfStatementEval = do
  withTokens "if statement eval" "if false { x: int = 1; }" $ \ast -> do
    result <- evalParsed "if statement eval" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Nothing -> return ()
          other -> error $ "expected x to be absent, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testIfElseStatementEval :: IO ()
testIfElseStatementEval = do
  withTokens "if else statement eval" "if false { x: int = 1; } else { y: float = 42.0; }" $ \ast -> do
    result <- evalParsed "if else statement eval" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Nothing -> return ()
          other -> error $ "expected x to be absent, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testUnaryNegEval :: IO ()
testUnaryNegEval = do
  withTokens "unary negation eval" "x: int = -42;" $ \ast -> do
    result <- evalParsed "unary negation eval" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VInt Signed I32 (-42)) -> return ()
          other -> error $ "expected x = -42, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testUnaryNotEval :: IO ()
testUnaryNotEval = do
  withTokens "unary not eval" "x: bool = !false;" $ \ast -> do
    result <- evalParsed "unary not eval" ast
    case result of
      Right (_, env) ->
        case lookupValue "x" env of
          Just (VBool True) -> return ()
          other -> error $ "expected x = true, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testFunctionEval :: IO ()
testFunctionEval = do
  withTokens "function eval" "add :: fn(a: i32, b: i32) -> i32 { a + b }; result: i32 = add(1, 2);" $ \ast -> do
    result <- evalParsed "function eval" ast
    case result of
      Right (_, env) ->
        case lookupValue "result" env of
          Just (VInt Signed I32 3) -> return ()
          other -> error $ "expected result = 3, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testClosureCapturesByValue :: IO ()
testClosureCapturesByValue = do
  -- Function items do not capture variables declared in the same block.
  checkErr "closure does not capture same-block variable" "x :: 10; addX :: fn(y: i32) -> i32 { x + y }; x :: 20; result: i32 = addX(5);"

testFunctionLocalScope :: IO ()
testFunctionLocalScope = do
  withTokens "function local scope" "f :: fn() -> i32 { x :: 1; x }; result: i32 = f();" $ \ast -> do
    result <- evalParsed "function local scope" ast
    case result of
      Right (_, env) -> do
        case lookupValue "result" env of
          Just (VInt Signed I32 1) -> return ()
          other -> error $ "expected result = 1, got " ++ show other
        case lookupValue "x" env of
          Nothing -> return ()
          other -> error $ "expected function local x to be absent, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testDirectRecursion :: IO ()
testDirectRecursion = do
  withTokens "direct recursion" "fact :: fn(n: i32) -> i32 { if n == 0 { 1 } else { n * fact(n - 1) } }; result: i32 = fact(5);" $ \ast -> do
    result <- evalParsed "direct recursion" ast
    case result of
      Right (_, env) ->
        case lookupValue "result" env of
          Just (VInt Signed I32 120) -> return ()
          other -> error $ "expected result = 120, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testTopLevelMutualRecursion :: IO ()
testTopLevelMutualRecursion = do
  withTokens "top-level mutual recursion" "even :: fn(n: i32) -> bool { if n == 0 { true } else { odd(n - 1) } }; odd :: fn(n: i32) -> bool { if n == 0 { false } else { even(n - 1) } }; result: bool = even(10);" $ \ast -> do
    result <- evalParsed "top-level mutual recursion" ast
    case result of
      Right (_, env) ->
        case lookupValue "result" env of
          Just (VBool True) -> return ()
          other -> error $ "expected result = true, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testLocalMutualRecursionRejected :: IO ()
testLocalMutualRecursionRejected = do
  checkErr "local mutual recursion rejected" "outer :: fn(n: i32) -> bool { even :: fn(x: i32) -> bool { if x == 0 { true } else { odd(x - 1) } }; odd :: fn(x: i32) -> bool { if x == 0 { false } else { even(x - 1) } }; even(n) }; result: bool = outer(9);"

testForwardFunctionReference :: IO ()
testForwardFunctionReference = do
  withTokens "forward function reference" "result: i32 = f(); f :: fn() -> i32 { 42 };" $ \ast -> do
    result <- evalParsed "forward function reference" ast
    case result of
      Right (_, env) ->
        case lookupValue "result" env of
          Just (VInt Signed I32 42) -> return ()
          other -> error $ "expected result = 42, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testFunctionUsesGlobal :: IO ()
testFunctionUsesGlobal = do
  withTokens "function uses global" "x :: 10; f :: fn() -> i32 { x }; result: i32 = f();" $ \ast -> do
    result <- evalParsed "function uses global" ast
    case result of
      Right (_, env) ->
        case lookupValue "result" env of
          Just (VInt Signed I32 10) -> return ()
          other -> error $ "expected result = 10, got " ++ show other
      Left err -> error $ "eval failed: " ++ show err

testReplRecursiveFunction :: IO ()
testReplRecursiveFunction = do
  decl <- assertRight "parse repl recursive function" $ runAlex "succ :: fn(n:int) -> int {if n == 0 { 1 } else { 1 + succ(n-1) }}" parseRepl
  expr <- assertRight "parse repl recursive call" $ runAlex "succ(3)" parseRepl
  case checkReplInputWithEnv [] decl of
    Left err -> error $ "repl recursive function type check failed: " ++ show err
    Right (typedDecl, typeEnv) ->
      case runExcept (runStateT (evalReplInput typedDecl) emptyEnv) of
        Left err -> error $ "repl recursive function eval failed: " ++ show err
        Right (_, env) ->
          case checkReplInputWithEnv typeEnv expr of
            Left err -> error $ "repl recursive call type check failed: " ++ show err
            Right (typedExpr, _) ->
              case runExcept (runStateT (evalReplInput typedExpr) env) of
                Right (VInt Signed I32 4, _) -> return ()
                Right (val, _) -> error $ "expected succ(3) = 4, got " ++ show val
                Left err -> error $ "repl recursive call eval failed: " ++ show err

testDuplicateParamsRejected :: IO ()
testDuplicateParamsRejected = do
  checkErr "duplicate params" "f :: fn(x: i32, x: i32) -> i32 { x };"

testDuplicateFunctionsRejected :: IO ()
testDuplicateFunctionsRejected = do
  checkErr "duplicate functions" "f :: fn() -> i32 { 1 }; f :: fn() -> i32 { 2 };"

testDuplicateDeclarationsRejected :: IO ()
testDuplicateDeclarationsRejected = do
  checkErr "duplicate declarations" "x :: 1; x :: 2;"

testNestedFunctionCaptureRejected :: IO ()
testNestedFunctionCaptureRejected = do
  checkErr "nested function capture rejected" "outer :: fn(x: i32) -> i32 { helper :: fn(n: i32) -> i32 { if n == 0 { x } else { helper(n - 1) } }; helper(3) }; result: i32 = outer(7);"

pos0 :: Lexer.AlexPosn
pos0 = Lexer.AlexPn 0 1 1

identFrom :: String -> Ident
identFrom name = Ident pos0 name
