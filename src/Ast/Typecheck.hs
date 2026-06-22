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
import Data.Maybe (mapMaybe)
import Lexer (AlexPosn (..))

data Error
  = TypeMismatch AlexPosn Type Type
  | UnsupportedOp AlexPosn BinaryOp Type Type
  | UnsupportedUnaryOp AlexPosn UnaryOp Type
  | MissingAnnotation AlexPosn
  | DuplicateParams AlexPosn String
  | DuplicateFunction AlexPosn String
  | DuplicateDeclaration AlexPosn String
  | UndefinedVariable AlexPosn String
  | NotCallable AlexPosn Type
  | ArityMismatch AlexPosn Int Int
  deriving (Show, Eq)

type TypeEnv = [Map String Type]

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

-- Environment helpers

emptyEnv :: TypeEnv
emptyEnv = []

lookupName :: String -> TypeEnv -> Maybe Type
lookupName _ [] = Nothing
lookupName name (scope : rest) =
  case Map.lookup name scope of
    Just typ -> Just typ
    Nothing -> lookupName name rest

bindInCurrentScope :: String -> Type -> TypeEnv -> TypeEnv
bindInCurrentScope name typ [] = [Map.singleton name typ]
bindInCurrentScope name typ (scope : rest) =
  Map.insert name typ scope : rest

globalEnv :: TypeEnv -> TypeEnv
globalEnv [] = []
globalEnv [scope] = [scope]
globalEnv (_ : rest) = globalEnv rest

pushScope :: TypeEnv -> TypeEnv
pushScope env = Map.empty : env

popScope :: TypeEnv -> TypeEnv
popScope [] = []
popScope (_ : rest) = rest

withScope :: Check a -> Check a
withScope action = do
  modify pushScope
  result <- action
  modify popScope
  return result

-- Expressions

checkExpr :: ParsedExpr -> Either Error TypedExpr
checkExpr expr = fst <$> runCheck emptyEnv (checkExprM expr)

checkExprM :: ParsedExpr -> Check TypedExpr
checkExprM (Expr pos k) = case k of
  IntLit i -> return $ Expr (IntType pos Signed I32) (IntLit i)
  FloatLit f -> return $ Expr (FloatType pos F64) (FloatLit f)
  BoolLit b -> return $ Expr (BoolType pos) (BoolLit b)
  UnaryExpr op e -> checkUnaryExpr pos op e
  BinaryExpr op l r -> checkBinaryExpr pos op l r
  VarExpr i@(Ident _ name) -> do
    env <- get
    case lookupName name env of
      Just typ -> return $ Expr typ (VarExpr i)
      Nothing -> liftEither $ Left $ UndefinedVariable pos name
  IfExpr c t e -> checkIfExpr pos c t e
  FnExpr params ret body -> checkFnExpr pos params ret body
  CallExpr callee args -> checkCallExpr pos callee args

checkIfExpr :: AlexPosn -> ParsedExpr -> ParsedBlock -> Maybe ParsedBlock -> Check TypedExpr
checkIfExpr pos c t mElse = do
  tc <- checkExprM c
  unless (isBoolLike (typeOf tc)) $ liftEither $ Left $ TypeMismatch pos (typeOf tc) (BoolType pos)

  tt <- withScope (checkBlockM t)
  mt <- traverse (withScope . checkBlockM) mElse
  ty <- liftEither $ mergeTypes pos (blockType tt) (maybe UnitType blockType mt)

  return $ Expr ty (IfExpr tc tt mt)
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
  put (pushScope (globalEnv env))
  mapM_ bindParam params
  typedBody <- checkBlockM body
  put env
  unless (compatible ret (blockType typedBody)) $ liftEither $ Left $ TypeMismatch pos ret (blockType typedBody)
  return $ Expr (FnType pos (map paramType params) ret) (FnExpr params ret typedBody)

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

checkOpWith :: (Type -> Type -> Bool) -> (AlexPosn -> Type -> Type -> Type) -> AlexPosn -> BinaryOp -> Type -> Type -> Either Error Type
checkOpWith predicate result pos op t1 t2
  | predicate t1 t2 = Right (result pos t1 t2)
  | otherwise = Left (UnsupportedOp pos op t1 t2)

commonResult :: AlexPosn -> Type -> Type -> Type
commonResult pos t1 t2 = case (t1, t2) of
  (IntType _ s k, IntType _ _ _) -> IntType pos s k
  (FloatType _ k, FloatType _ _) -> FloatType pos k
  (IntType _ _ _, FloatType _ _) -> FloatType pos F64
  (FloatType _ _, IntType _ _ _) -> FloatType pos F64
  _ -> t1

checkBinaryOp :: AlexPosn -> BinaryOp -> Type -> Type -> Either Error Type
checkBinaryOp pos op t1 t2 = case op of
  AddOp -> numeric
  SubOp -> numeric
  MulOp -> numeric
  DivOp -> numeric
  AndOp -> boolLike
  OrOp -> boolLike
  EqOp -> equality
  NeqOp -> equality
  LtOp -> ordered
  LeqOp -> ordered
  GtOp -> ordered
  GeqOp -> ordered
  where
    numeric = checkOpWith numericCompatible commonResult pos op t1 t2
    boolLike = checkOpWith (\a b -> isBoolLike a && isBoolLike b) (\_ _ _ -> BoolType pos) pos op t1 t2
    equality = checkOpWith (\a b -> isBoolLike a && isBoolLike b || numericCompatible a b) (\_ _ _ -> BoolType pos) pos op t1 t2
    ordered = checkOpWith numericCompatible (\_ _ _ -> BoolType pos) pos op t1 t2

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
checkStmt stmt = fst <$> runCheck emptyEnv (checkStmtM stmt)

checkStmtM :: ParsedStmt -> Check TypedStmt
checkStmtM (Stmt _ k) = case k of
  DeclStmt decl -> Stmt UnitType . DeclStmt <$> checkDecl decl
  ExprStmt expr -> Stmt UnitType . ExprStmt <$> checkExprM expr

checkFunctionItemStmt :: ParsedStmt -> Check TypedStmt
checkFunctionItemStmt (Stmt _ (DeclStmt (ValueDecl m ident mt (Just expr@(Expr _ (FnExpr {})))))) = do
  te <- checkExprM expr
  let inferred = typeOf te
  typ <- case mt of
    Nothing -> return inferred
    Just t -> do
      unless (compatible t inferred) $ liftEither $ Left $ TypeMismatch (identPos ident) t inferred
      return t
  return $ Stmt UnitType $ DeclStmt $ ValueDecl m ident (Just typ) (Just te)
checkFunctionItemStmt stmt = checkStmtM stmt

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
bindIdent (Ident pos name) typ = do
  env <- get
  case env of
    scope : _ | Map.member name scope -> liftEither $ Left $ DuplicateDeclaration pos name
    _ -> modify (bindInCurrentScope name typ)

bindParam :: Param -> Check ()
bindParam (Param ident typ) = bindIdent ident typ

paramType :: Param -> Type
paramType (Param _ typ) = typ

-- Function items

isFunctionItem :: Stmt annot -> Bool
isFunctionItem = \case
  Stmt _ (DeclStmt (ValueDecl Constant _ _ (Just (Expr _ (FnExpr {}))))) -> True
  _ -> False

installFunctionItems :: [ParsedStmt] -> Check ()
installFunctionItems stmts = do
  let fns = functionItems stmts
  checkDuplicateFunctions (fnItemsPos fns) (map (\(Ident _ name, _) -> name) fns)
  modify pushScope
  mapM_ (\(Ident _ name, typ) -> modify (bindInCurrentScope name typ)) fns

functionItems :: [ParsedStmt] -> [(Ident, Type)]
functionItems = mapMaybe $ \case
  Stmt _ (DeclStmt (ValueDecl Constant (Ident pos name) _ (Just (Expr _ (FnExpr params ret _))))) ->
    Just (Ident pos name, FnType pos (map paramType params) ret)
  _ -> Nothing

fnItemsPos :: [(Ident, Type)] -> AlexPosn
fnItemsPos = \case
  [] -> AlexPn 0 1 1
  ((Ident pos _, _) : _) -> pos

checkDuplicateFunctions :: AlexPosn -> [String] -> Check ()
checkDuplicateFunctions pos names = go names
  where
    go [] = return ()
    go (x : xs)
      | x `elem` xs = liftEither $ Left (DuplicateFunction pos x)
      | otherwise = go xs

-- Blocks

checkBlock :: ParsedBlock -> Either Error TypedBlock
checkBlock block = fst <$> runCheck emptyEnv (checkBlockM block)

checkBlockM :: ParsedBlock -> Check TypedBlock
checkBlockM (Block stmts expr) = do
  outerEnv <- get
  installFunctionItems stmts
  fnScope <- get
  typedStmts <- mapM (checkStmtInBlock outerEnv fnScope) stmts
  typedExpr <- traverse checkExprM expr
  put outerEnv
  return $ Block typedStmts typedExpr

checkStmtInBlock :: TypeEnv -> TypeEnv -> ParsedStmt -> Check TypedStmt
checkStmtInBlock _ fnScope stmt
  | isFunctionItem stmt = do
      saveEnv <- get
      put fnScope
      typed <- checkFunctionItemStmt stmt
      put saveEnv
      return typed
  | otherwise = checkStmtM stmt

blockType :: TypedBlock -> Type
blockType (Block _ Nothing) = UnitType
blockType (Block _ (Just expr)) = typeOf expr

-- Program

checkProgram :: ParsedProgram -> Either Error ()
checkProgram = void . checkProgramTyped

checkProgramTyped :: ParsedProgram -> Either Error TypedProgram
checkProgramTyped (Program toplevels) =
  fst
    <$> runCheck
      emptyEnv
      ( do
          let stmts = map topLevelStmt toplevels
          installFunctionItems stmts
          typedTopLevels <- mapM checkTopLevel toplevels
          return $ Program typedTopLevels
      )
  where
    topLevelStmt (TopLevelStmt stmt) = stmt

checkTopLevel :: ParsedTopLevel -> Check TypedTopLevel
checkTopLevel (TopLevelStmt stmt)
  | isFunctionItem stmt = TopLevelStmt <$> checkFunctionItemStmt stmt
  | otherwise = TopLevelStmt <$> checkStmtM stmt

-- REPL

checkReplInput :: ParsedRepl -> Either Error TypedRepl
checkReplInput input = fst <$> checkReplInputWithEnv emptyEnv input

checkReplInputWithEnv :: TypeEnv -> ParsedRepl -> Either Error (TypedRepl, TypeEnv)
checkReplInputWithEnv env input = runCheck env $ case input of
  ReplStmt stmt
    | isFunctionItem stmt -> do
        let fns = functionItems [stmt]
        checkDuplicateFunctions (fnItemsPos fns) (map (\(Ident _ name, _) -> name) fns)
        mapM_ (\(ident, typ) -> bindIdent ident typ) fns
        ReplStmt <$> checkFunctionItemStmt stmt
    | otherwise -> ReplStmt <$> checkStmtM stmt
  ReplExpr expr -> ReplExpr <$> checkExprM expr
