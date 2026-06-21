module Ast.Typecheck
  ( Error (..),
    TypedExpr,
    TypedDecl,
    TypedBlock,
    TypedStmt,
    TypedRepl,
    TypedProgram,
    TypedTopLevel,
    TypeEnv,
    checkExpr,
    checkStmt,
    checkBlock,
    checkProgram,
    checkProgramTyped,
    checkReplInput,
    checkReplInputWithEnv,
  )
where

import Ast.Syntax
import Control.Monad (unless)
import Control.Monad.Except (liftEither)
import Control.Monad.State (StateT (..), get, modify, put, runStateT)
import Data.Functor (void)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Lexer (AlexPosn)

data Error
  = TypeMismatch AlexPosn Type Type
  | UnsupportedOp AlexPosn BinaryOp Type Type
  | UnsupportedUnaryOp AlexPosn UnaryOp Type
  | MissingAnnotation AlexPosn
  | DuplicateParams AlexPosn String
  | UndefinedVariable AlexPosn String
  | NotCallable AlexPosn Type
  | ArityMismatch AlexPosn Int Int
  deriving (Show, Eq)

type TypeEnv = Map String Type

type Check a = StateT TypeEnv (Either Error) a

type TypedExpr = Expr Type

type TypedDecl = Decl Type

type TypedBlock = Block Type

type TypedStmt = Stmt Type

type TypedRepl = Repl Type

type TypedProgram = Program Type

type TypedTopLevel = TopLevel Type

typeOf :: TypedExpr -> Type
typeOf = exprAnnot

runCheck :: TypeEnv -> Check a -> Either Error (a, TypeEnv)
runCheck = flip runStateT

-- Expressions

checkExpr :: ParsedExpr -> Either Error TypedExpr
checkExpr expr = fst <$> runCheck Map.empty (checkExprM expr)

checkExprM :: ParsedExpr -> Check TypedExpr
checkExprM (Expr pos k) = case k of
  IntLit i -> return $ Expr (IntType pos Signed I32) (IntLit i)
  FloatLit f -> return $ Expr (FloatType pos F64) (FloatLit f)
  BoolLit b -> return $ Expr (BoolType pos) (BoolLit b)
  UnaryExpr op e -> checkUnaryExpr pos op e
  BinaryExpr op l r -> checkBinaryExpr pos op l r
  VarExpr i@(Ident _ name) -> do
    env <- get
    case Map.lookup name env of
      Just typ -> return $ Expr typ (VarExpr i)
      Nothing -> liftEither $ Left $ UndefinedVariable pos name
  IfExpr c t e -> checkIfExpr pos c t e
  FnExpr params ret body -> checkFnExpr pos params ret body
  CallExpr callee args -> checkCallExpr pos callee args

checkIfExpr :: AlexPosn -> ParsedExpr -> ParsedBlock -> Maybe ParsedBlock -> Check TypedExpr
checkIfExpr pos c t e = do
  tc <- checkExprM c
  unless (isBoolLike (typeOf tc)) $ liftEither $ Left $ TypeMismatch pos (typeOf tc) (BoolType pos)

  tt <- checkScopedBlock t
  mte <- traverse checkScopedBlock e
  ty <- liftEither $ mergeTypes pos (blockType tt) (maybe UnitType blockType mte)

  return $ Expr ty (IfExpr tc tt mte)
  where
    mergeTypes p t1 t2
      | compatible t1 t2 = Right t1
      | otherwise = Left $ TypeMismatch p t1 t2

checkUnaryExpr :: AlexPosn -> UnaryOp -> ParsedExpr -> Check TypedExpr
checkUnaryExpr pos op e = do
  te <- checkExprM e
  let tye = typeOf te
  liftEither $ case op of
    NegOp -> if isNumeric tye then Right $ Expr tye (UnaryExpr op te) else Left $ UnsupportedUnaryOp pos op tye
    NotOp -> if isBoolLike tye then Right $ Expr tye (UnaryExpr op te) else Left $ UnsupportedUnaryOp pos op tye
    AmpersandOp -> Left $ UnsupportedUnaryOp pos op tye

checkFnExpr :: AlexPosn -> [Param] -> Type -> ParsedBlock -> Check TypedExpr
checkFnExpr pos params ret body = do
  checkDuplicateParams params
  env <- get
  mapM_ bindParam params
  typedBody <- checkBlockM body
  put env
  unless (compatible ret (blockType typedBody)) $ liftEither $ Left $ TypeMismatch pos ret (blockType typedBody)
  return $ Expr (FnType pos (map paramType params) ret) (FnExpr params ret typedBody)

checkFnSig :: AlexPosn -> [Param] -> Type -> Check ()
checkFnSig pos params ret = do
  checkDuplicateParams params
  env <- get
  mapM_ bindParam params
  put env

checkDuplicateParams :: [Param] -> Check ()
checkDuplicateParams = go Map.empty
  where
    go _ [] = return ()
    go seen (Param (Ident pos name) _ : rest)
      | Map.member name seen = liftEither $ Left $ DuplicateParams pos name
      | otherwise = go (Map.insert name () seen) rest

checkCallExpr :: AlexPosn -> ParsedExpr -> [ParsedExpr] -> Check TypedExpr
checkCallExpr pos callee args = do
  typedCallee <- checkExprM callee
  typedArgs <- mapM checkExprM args
  case typeOf typedCallee of
    FnType _ paramTypes ret -> do
      unless (length paramTypes == length typedArgs) $ liftEither $ Left $ ArityMismatch pos (length paramTypes) (length typedArgs)
      mapM_ checkArg (zip paramTypes typedArgs)
      return $ Expr ret (CallExpr typedCallee typedArgs)
    other -> liftEither $ Left $ NotCallable pos other
  where
    checkArg (expected, actual) =
      unless (compatible expected (typeOf actual)) $ liftEither $ Left $ TypeMismatch pos expected (typeOf actual)

checkBinaryExpr :: AlexPosn -> BinaryOp -> ParsedExpr -> ParsedExpr -> Check TypedExpr
checkBinaryExpr pos op l r = do
  tl <- checkExprM l
  tr <- checkExprM r
  ty <- liftEither $ checkBinaryOp pos op (typeOf tl) (typeOf tr)
  return $ Expr ty (BinaryExpr op tl tr)

checkBinaryOp :: AlexPosn -> BinaryOp -> Type -> Type -> Either Error Type
checkBinaryOp pos op t1 t2 = case op of
  AddOp -> checkNumericOp pos op t1 t2
  SubOp -> checkNumericOp pos op t1 t2
  MulOp -> checkNumericOp pos op t1 t2
  DivOp -> checkNumericOp pos op t1 t2
  AndOp -> checkBoolOp pos op t1 t2
  OrOp -> checkBoolOp pos op t1 t2
  EqOp -> checkEqualityOp pos op t1 t2
  NeqOp -> checkEqualityOp pos op t1 t2
  LtOp -> checkOrderedOp pos op t1 t2
  LeqOp -> checkOrderedOp pos op t1 t2
  GtOp -> checkOrderedOp pos op t1 t2
  GeqOp -> checkOrderedOp pos op t1 t2

checkNumericOp :: AlexPosn -> BinaryOp -> Type -> Type -> Either Error Type
checkNumericOp pos op t1 t2
  | numericCompatible t1 t2 = Right numT
  | otherwise = Left (UnsupportedOp pos op t1 t2)
  where
    numT = case (t1, t2) of
      (IntType _ s k, IntType _ _ _) -> IntType pos s k
      (FloatType _ k, FloatType _ _) -> FloatType pos k
      (IntType _ _ _, FloatType _ _) -> FloatType pos F64
      (FloatType _ _, IntType _ _ _) -> FloatType pos F64
      _ -> t1

checkBoolOp :: AlexPosn -> BinaryOp -> Type -> Type -> Either Error Type
checkBoolOp pos op t1 t2
  | isBoolLike t1 && isBoolLike t2 = Right (BoolType pos)
  | otherwise = Left (UnsupportedOp pos op t1 t2)

checkEqualityOp :: AlexPosn -> BinaryOp -> Type -> Type -> Either Error Type
checkEqualityOp pos op t1 t2
  | isBoolLike t1 && isBoolLike t2 = Right (BoolType pos)
  | numericCompatible t1 t2 = Right (BoolType pos)
  | otherwise = Left (UnsupportedOp pos op t1 t2)

checkOrderedOp :: AlexPosn -> BinaryOp -> Type -> Type -> Either Error Type
checkOrderedOp pos op t1 t2
  | numericCompatible t1 t2 = Right (BoolType pos)
  | otherwise = Left (UnsupportedOp pos op t1 t2)

numericCompatible :: Type -> Type -> Bool
numericCompatible t1 t2 = case (t1, t2) of
  (IntType _ s1 k1, IntType _ s2 k2) -> s1 == s2 && k1 == k2
  (FloatType _ k1, FloatType _ k2) -> k1 == k2
  (TypeName _, _) -> True
  (_, TypeName _) -> True
  _ -> False

compatible :: Type -> Type -> Bool
compatible t1 t2 = case (t1, t2) of
  (IntType _ s1 k1, IntType _ s2 k2) -> s1 == s2 && k1 == k2
  (FloatType _ k1, FloatType _ k2) -> k1 == k2
  (BoolType _, BoolType _) -> True
  (FnType _ ps1 r1, FnType _ ps2 r2) -> length ps1 == length ps2 && and (zipWith compatible ps1 ps2) && compatible r1 r2
  (UnitType, UnitType) -> True
  (TypeName _, _) -> True
  (_, TypeName _) -> True
  (_, _) -> False

isNumeric :: Type -> Bool
isNumeric = \case
  IntType {} -> True
  FloatType {} -> True
  TypeName _ -> True
  _ -> False

isBoolLike :: Type -> Bool
isBoolLike = \case
  BoolType _ -> True
  TypeName _ -> True
  _ -> False

-- Statements

checkStmt :: ParsedStmt -> Either Error TypedStmt
checkStmt stmt = fst <$> runCheck Map.empty (checkStmtM stmt)

checkStmtM :: ParsedStmt -> Check TypedStmt
checkStmtM (Stmt _ k) = case k of
  DeclStmt decl -> Stmt UnitType . DeclStmt <$> checkDecl decl
  ExprStmt expr -> Stmt UnitType . ExprStmt <$> checkExprM expr

checkDecl :: ParsedDecl -> Check TypedDecl
checkDecl = \case
  ValueDecl _ ident Nothing Nothing -> liftEither $ Left (MissingAnnotation (identPos ident))
  ValueDecl mut ident Nothing (Just expr) -> do
    typedExpr <- checkExprM expr
    let inferred = typeOf typedExpr
    bindIdent ident inferred
    return $ ValueDecl mut ident (Just inferred) (Just typedExpr)
  ValueDecl m i (Just t) Nothing -> do
    bindIdent i t
    return $ ValueDecl m i (Just t) Nothing
  ValueDecl m i (Just t) (Just e) -> do
    te <- checkExprM e
    let inferred = typeOf te
    unless (compatible t inferred) $ liftEither $ Left $ TypeMismatch (identPos i) t inferred
    bindIdent i t
    return $ ValueDecl m i (Just t) (Just te)

identPos :: Ident -> AlexPosn
identPos (Ident pos _) = pos

bindIdent :: Ident -> Type -> Check ()
bindIdent (Ident _ name) typ = modify (Map.insert name typ)

bindParam :: Param -> Check ()
bindParam (Param ident typ) = bindIdent ident typ

paramType :: Param -> Type
paramType (Param _ typ) = typ

-- Blocks

checkBlock :: ParsedBlock -> Either Error TypedBlock
checkBlock block = fst <$> runCheck Map.empty (checkBlockM block)

checkBlockM :: ParsedBlock -> Check TypedBlock
checkBlockM (Block stmts expr) = do
  typedStmts <- mapM checkStmtM stmts
  typedExpr <- traverse checkExprM expr
  return $ Block typedStmts typedExpr

checkScopedBlock :: ParsedBlock -> Check TypedBlock
checkScopedBlock block = do
  env <- get
  typedBlock <- checkBlockM block
  put env
  return typedBlock

blockType :: TypedBlock -> Type
blockType (Block _ Nothing) = UnitType
blockType (Block _ (Just expr)) = typeOf expr

-- Program

-- TODO: Extending checking to identify `main` entry point.

checkProgram :: ParsedProgram -> Either Error ()
checkProgram = void . checkProgramTyped

checkProgramTyped :: ParsedProgram -> Either Error TypedProgram
checkProgramTyped (Program toplevels) = fst <$> runCheck Map.empty (Program <$> mapM checkTopLevel toplevels)

checkTopLevel :: ParsedTopLevel -> Check TypedTopLevel
checkTopLevel (TopLevelStmt stmt) = TopLevelStmt <$> checkStmtM stmt

-- REPL

checkReplInput :: ParsedRepl -> Either Error TypedRepl
checkReplInput input = fst <$> checkReplInputWithEnv Map.empty input

checkReplInputWithEnv :: TypeEnv -> ParsedRepl -> Either Error (TypedRepl, TypeEnv)
checkReplInputWithEnv env input = runCheck env $ case input of
  ReplStmt stmt -> ReplStmt <$> checkStmtM stmt
  ReplExpr expr -> ReplExpr <$> checkExprM expr
