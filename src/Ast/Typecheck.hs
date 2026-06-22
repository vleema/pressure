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
    errorPos,
    errorInfo,
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
  | UndefinedType AlexPosn String
  | NotCallable AlexPosn Type
  | ArityMismatch AlexPosn Int Int
  | AssignToConstant AlexPosn String
  deriving (Show, Eq)

errorPos :: Error -> AlexPosn
errorPos = \case
  TypeMismatch pos _ _ -> pos
  UnsupportedOp pos _ _ _ -> pos
  UnsupportedUnaryOp pos _ _ -> pos
  MissingAnnotation pos -> pos
  DuplicateParams pos _ -> pos
  DuplicateFunction pos _ -> pos
  DuplicateDeclaration pos _ -> pos
  UndefinedVariable pos _ -> pos
  UndefinedType pos _ -> pos
  NotCallable pos _ -> pos
  ArityMismatch pos _ _ -> pos
  AssignToConstant pos _ -> pos

errorInfo :: Error -> (AlexPosn, String)
errorInfo err =
  ( errorPos err,
    case err of
      TypeMismatch _ expected actual -> "type mismatch: expected '" ++ prettyType expected ++ "', found '" ++ prettyType actual ++ "'"
      UnsupportedOp _ op t1 t2 -> "cannot use operator '" ++ prettyBinaryOp op ++ "' on type '" ++ prettyType t1 ++ "' and '" ++ prettyType t2 ++ "'"
      UnsupportedUnaryOp _ op t -> "cannot use unary operator '" ++ prettyUnaryOp op ++ "' on type '" ++ prettyType t ++ "'"
      MissingAnnotation _ -> "missing type annotation"
      DuplicateParams _ name -> "duplicate parameter '" ++ name ++ "'"
      DuplicateFunction _ name -> "duplicate function '" ++ name ++ "'"
      DuplicateDeclaration _ name -> "duplicate declaration '" ++ name ++ "'"
      UndefinedVariable _ name -> "undefined variable '" ++ name ++ "'"
      UndefinedType _ name -> "undefined type '" ++ name ++ "'"
      NotCallable _ t -> "cannot call value of type '" ++ prettyType t ++ "'"
      ArityMismatch _ expected actual -> "wrong number of arguments: expected " ++ show expected ++ ", got " ++ show actual
      AssignToConstant _ name -> "cannot assign to constant '" ++ name ++ "'"
  )

type TypeEnv = [Map String (Type, Mutability)]

type Check a = StateT TypeEnv (Either Error) a

typeOf :: TypedExpr -> Type
typeOf = typedExprType

runCheck :: TypeEnv -> Check a -> Either Error (a, TypeEnv)
runCheck = flip runStateT

checkType :: TypeSyntax -> Check Type
checkType (TypeSyntax pos k) = case k of
  NameSyntax name -> liftEither $ Left $ UndefinedType pos name
  BoolSyntax -> return BoolT
  IntSyntax sign size -> return $ IntT sign size
  FloatSyntax size -> return $ FloatT size
  FnSyntax params ret -> FnT <$> mapM checkType params <*> checkType ret
  UnitSyntax -> return UnitT

-- Environment helpers

emptyEnv :: TypeEnv
emptyEnv = []

lookupName :: String -> TypeEnv -> Maybe (Type, Mutability)
lookupName _ [] = Nothing
lookupName name (scope : rest) =
  case Map.lookup name scope of
    Just tm -> Just tm
    Nothing -> lookupName name rest

bindInCurrentScope :: String -> (Type, Mutability) -> TypeEnv -> TypeEnv
bindInCurrentScope name tm [] = [Map.singleton name tm]
bindInCurrentScope name tm (scope : rest) =
  Map.insert name tm scope : rest

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
checkExprM (ParsedExpr pos k) = case k of
  ParsedIntLit i -> return $ TypedExpr pos (IntT Signed I32) (TypedIntLit i)
  ParsedFloatLit f -> return $ TypedExpr pos (FloatT F64) (TypedFloatLit f)
  ParsedBoolLit b -> return $ TypedExpr pos BoolT (TypedBoolLit b)
  ParsedUnaryExpr op e -> checkUnaryExpr pos op e
  ParsedBinaryExpr op l r -> checkBinaryExpr pos op l r
  ParsedVarExpr i@(Ident _ name) -> do
    env <- get
    case lookupName name env of
      Just (typ, _) -> return $ TypedExpr pos typ (TypedVarExpr i)
      Nothing -> liftEither $ Left $ UndefinedVariable pos name
  ParsedIfExpr c t e -> checkIfExpr pos c t e
  ParsedFnExpr params ret body -> checkFnExpr pos params ret body
  ParsedCallExpr callee args -> checkCallExpr pos callee args

checkIfExpr :: AlexPosn -> ParsedExpr -> ParsedBlock -> Maybe ParsedBlock -> Check TypedExpr
checkIfExpr pos c t mElse = do
  tc <- checkExprM c
  unless (isBoolLike (typeOf tc)) $ liftEither $ Left $ TypeMismatch pos (typeOf tc) BoolT
  tt <- withScope (checkBlockM t)
  mt <- traverse (withScope . checkBlockM) mElse
  ty <- liftEither $ mergeTypes pos (blockType tt) (maybe UnitT blockType mt)
  return $ TypedExpr pos ty (TypedIfExpr tc tt mt)
  where
    mergeTypes p t1 t2
      | compatible t1 t2 = Right t1
      | otherwise = Left $ TypeMismatch p t1 t2

checkUnaryExpr :: AlexPosn -> UnaryOp -> ParsedExpr -> Check TypedExpr
checkUnaryExpr pos op e = do
  te <- checkExprM e
  let tye = typeOf te
  liftEither $ case op of
    NegOp -> if isNumeric tye then Right $ TypedExpr pos tye (TypedUnaryExpr op te) else Left $ UnsupportedUnaryOp pos op tye
    NotOp -> if isBoolLike tye then Right $ TypedExpr pos tye (TypedUnaryExpr op te) else Left $ UnsupportedUnaryOp pos op tye
    AmpersandOp -> Left $ UnsupportedUnaryOp pos op tye

checkFnExpr :: AlexPosn -> [Param] -> TypeSyntax -> ParsedBlock -> Check TypedExpr
checkFnExpr pos params ret body = do
  checkDuplicateParams params
  typedRet <- checkType ret
  typedParams <- mapM checkParam params
  env <- get
  put (pushScope (globalEnv env))
  mapM_ bindTypedParam typedParams
  typedBody <- checkBlockM body
  put env
  unless (compatible typedRet (blockType typedBody)) $
    liftEither $
      Left $
        TypeMismatch pos typedRet (blockType typedBody)
  return $ TypedExpr pos (FnT (map typedParamType typedParams) typedRet) (TypedFnExpr typedParams typedRet typedBody)

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
    FnT paramTypes ret -> do
      unless (length paramTypes == length typedArgs) $
        liftEither $
          Left $
            ArityMismatch pos (length paramTypes) (length typedArgs)
      mapM_ checkArg (zip paramTypes typedArgs)
      return $ TypedExpr pos ret (TypedCallExpr typedCallee typedArgs)
    other -> liftEither $ Left $ NotCallable pos other
  where
    checkArg (expected, actual) =
      unless (compatible expected (typeOf actual)) $ liftEither $ Left $ TypeMismatch pos expected (typeOf actual)

checkBinaryExpr :: AlexPosn -> BinaryOp -> ParsedExpr -> ParsedExpr -> Check TypedExpr
checkBinaryExpr pos op l r = do
  tl <- checkExprM l
  tr <- checkExprM r
  ty <- liftEither $ checkBinaryOp pos op (typeOf tl) (typeOf tr)
  return $ TypedExpr pos ty (TypedBinaryExpr op tl tr)

checkOpWith :: (Type -> Type -> Bool) -> (Type -> Type -> Type) -> AlexPosn -> BinaryOp -> Type -> Type -> Either Error Type
checkOpWith predicate result pos op t1 t2
  | predicate t1 t2 = Right (result t1 t2)
  | otherwise = Left (UnsupportedOp pos op t1 t2)

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
    numeric = checkOpWith numericCompatible (\t _ -> t) pos op t1 t2
    boolLike = checkOpWith (\a b -> isBoolLike a && isBoolLike b) (\_ _ -> BoolT) pos op t1 t2
    equality = checkOpWith (\a b -> isBoolLike a && isBoolLike b || numericCompatible a b) (\_ _ -> BoolT) pos op t1 t2
    ordered = checkOpWith numericCompatible (\_ _ -> BoolT) pos op t1 t2

numericCompatible :: Type -> Type -> Bool
numericCompatible t1 t2 = case (t1, t2) of
  (IntT s1 k1, IntT s2 k2) -> s1 == s2 && k1 == k2
  (FloatT k1, FloatT k2) -> k1 == k2
  _ -> False

compatible :: Type -> Type -> Bool
compatible t1 t2 = case (t1, t2) of
  (IntT s1 k1, IntT s2 k2) -> s1 == s2 && k1 == k2
  (FloatT k1, FloatT k2) -> k1 == k2
  (BoolT, BoolT) -> True
  (UnitT, UnitT) -> True
  (FnT ps1 r1, FnT ps2 r2) -> length ps1 == length ps2 && and (zipWith compatible ps1 ps2) && compatible r1 r2
  (_, _) -> False

isNumeric :: Type -> Bool
isNumeric = \case
  IntT {} -> True
  FloatT {} -> True
  _ -> False

isBoolLike :: Type -> Bool
isBoolLike = \case
  BoolT -> True
  _ -> False

-- Statements

checkStmt :: ParsedStmt -> Either Error TypedStmt
checkStmt stmt = fst <$> runCheck emptyEnv (checkStmtM stmt)

checkStmtM :: ParsedStmt -> Check TypedStmt
checkStmtM (ParsedStmt pos k) = case k of
  ParsedDeclStmt decl -> TypedStmt pos . TypedDeclStmt <$> checkDecl decl
  ParsedExprStmt expr -> TypedStmt pos . TypedExprStmt <$> checkExprM expr
  ParsedAssignStmt assign -> TypedStmt pos . TypedAssignStmt <$> checkAssign assign

checkFunctionItemStmt :: ParsedStmt -> Check TypedStmt
checkFunctionItemStmt (ParsedStmt pos (ParsedDeclStmt (ParsedValueDecl m ident mts (Just expr@(ParsedExpr _ (ParsedFnExpr {})))))) = do
  te <- checkExprM expr
  let inferred = typeOf te
  typ <- case mts of
    Nothing -> return inferred
    Just ts -> do
      t <- checkType ts
      unless (compatible t inferred) $ liftEither $ Left $ TypeMismatch (identPos ident) t inferred
      return t
  return $ TypedStmt pos $ TypedDeclStmt $ TypedValueDecl m ident typ (Just te)
checkFunctionItemStmt stmt = checkStmtM stmt

checkDecl :: ParsedDecl -> Check TypedDecl
checkDecl = \case
  ParsedValueDecl _ ident Nothing Nothing -> liftEither $ Left (MissingAnnotation (identPos ident))
  ParsedValueDecl mut ident Nothing (Just expr) -> do
    typedExpr <- checkExprM expr
    let inferred = typeOf typedExpr
    bindIdent ident inferred mut
    return $ TypedValueDecl mut ident inferred (Just typedExpr)
  ParsedValueDecl mut i (Just ts) Nothing -> do
    t <- checkType ts
    bindIdent i t mut
    return $ TypedValueDecl mut i t Nothing
  ParsedValueDecl mut i (Just ts) (Just e) -> do
    t <- checkType ts
    te <- checkExprM e
    let inferred = typeOf te
    unless (compatible t inferred) $ liftEither $ Left $ TypeMismatch (identPos i) t inferred
    bindIdent i t mut
    return $ TypedValueDecl mut i t (Just te)

checkAssign :: ParsedAssign -> Check TypedAssign
checkAssign (ParsedAssign (Ident pos name) expr) = do
  env <- get
  case lookupName name env of
    Nothing -> liftEither $ Left $ UndefinedVariable pos name
    Just (_, Constant) -> liftEither $ Left $ AssignToConstant pos name
    Just (varT, Mutable) -> do
      typedExpr <- checkExprM expr
      unless (compatible varT (typeOf typedExpr)) $ liftEither $ Left $ TypeMismatch pos varT (typeOf typedExpr)
      return $ TypedAssign name typedExpr

bindIdent :: Ident -> Type -> Mutability -> Check ()
bindIdent (Ident pos name) typ mut = do
  env <- get
  case env of
    scope : _ | Map.member name scope -> liftEither $ Left $ DuplicateDeclaration pos name
    _ -> modify (bindInCurrentScope name (typ, mut))

checkParam :: Param -> Check TypedParam
checkParam (Param ident typ) = TypedParam ident <$> checkType typ

bindTypedParam :: TypedParam -> Check ()
bindTypedParam (TypedParam ident typ) = bindIdent ident typ Constant

typedParamType :: TypedParam -> Type
typedParamType (TypedParam _ typ) = typ

-- Function items

isFunctionItem :: ParsedStmt -> Bool
isFunctionItem = \case
  ParsedStmt _ (ParsedDeclStmt (ParsedValueDecl Constant _ _ (Just (ParsedExpr _ (ParsedFnExpr {}))))) -> True
  _ -> False

installFunctionItems :: [ParsedStmt] -> Check ()
installFunctionItems stmts = do
  let fns = functionItems stmts
  checkDuplicateFunctions (fnItemsPos fns) (map (identName . fnItemIdent) fns)
  typedFns <- mapM typeFunctionItem fns
  modify pushScope
  mapM_ (\(Ident _ name, typ) -> modify (bindInCurrentScope name (typ, Constant))) typedFns

data FunctionItem = FunctionItem Ident [Param] TypeSyntax

fnItemIdent :: FunctionItem -> Ident
fnItemIdent (FunctionItem ident _ _) = ident

functionItems :: [ParsedStmt] -> [FunctionItem]
functionItems = mapMaybe $ \case
  ParsedStmt _ (ParsedDeclStmt (ParsedValueDecl Constant ident _ (Just (ParsedExpr _ (ParsedFnExpr params ret _))))) ->
    Just (FunctionItem ident params ret)
  _ -> Nothing

typeFunctionItem :: FunctionItem -> Check (Ident, Type)
typeFunctionItem (FunctionItem ident params ret) = do
  typedParams <- mapM checkParam params
  typedRet <- checkType ret
  return (ident, FnT (map typedParamType typedParams) typedRet)

fnItemsPos :: [FunctionItem] -> AlexPosn
fnItemsPos = \case
  [] -> AlexPn 0 1 1
  (FunctionItem ident _ _ : _) -> identPos ident

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
  typedStmts <- mapM (checkStmtInBlock fnScope) stmts
  typedExpr <- traverse checkExprM expr
  put outerEnv
  return $ Block typedStmts typedExpr

checkStmtInBlock :: TypeEnv -> ParsedStmt -> Check TypedStmt
checkStmtInBlock fnScope stmt
  | isFunctionItem stmt = do
      saveEnv <- get
      put fnScope
      typed <- checkFunctionItemStmt stmt
      put saveEnv
      return typed
  | otherwise = checkStmtM stmt

blockType :: TypedBlock -> Type
blockType (Block _ Nothing) = UnitT
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
        checkDuplicateFunctions (fnItemsPos fns) (map (identName . fnItemIdent) fns)
        typedFns <- mapM typeFunctionItem fns
        mapM_ (\(ident, typ) -> bindIdent ident typ Constant) typedFns
        ReplStmt <$> checkFunctionItemStmt stmt
    | otherwise -> ReplStmt <$> checkStmtM stmt
  ReplExpr expr -> ReplExpr <$> checkExprM expr
