module Pressure.Typechecker.Check where

import Control.Monad (foldM, unless, when)
import Control.Monad.Except (liftEither)
import Control.Monad.State (MonadState (..), StateT (runStateT), put)
import Data.Functor (void)
import Data.Map qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Pressure.Builtins (checkAsCall, checkPrintfCall, initialTypeEnv, isAs, isPrintf)
import Pressure.Language.Ast
import Pressure.Language.Lexer (AlexPosn (..))
import Pressure.Language.Types
import Pressure.Typechecker.Env
import Pressure.Typechecker.Error

typeOf :: TypedExpr -> Type
typeOf = typedExprType

runCheck :: TypeEnv -> Check a -> Either Error (a, TypeEnv)
runCheck env action =
  case runStateT action (env, []) of
    Left err -> Left err
    Right (result, (finalEnv, _)) -> Right (result, finalEnv)

checkType :: TypeSyntax -> Check Type
checkType (TypeSyntax pos k) = case k of
  NameSyntax name -> liftEither $ Left $ UndefinedType pos name
  BoolSyntax -> return BoolT
  UnitSyntax -> return UnitT
  TySyntax -> return TypeT
  AnyTypeSyntax -> return AnyTypeT
  IntSyntax sign size -> return $ IntT sign size
  FloatSyntax size -> return $ FloatT size
  FnSyntax params ret -> FnT <$> mapM checkType params <*> checkType ret
  StringSyntax -> return StringT

-- Program

checkProgram :: ParsedProgram -> Either Error ()
checkProgram = void . checkProgramTyped

checkProgramTyped :: ParsedProgram -> Either Error TypedProgram
checkProgramTyped (Program toplevels) =
  fst
    <$> runCheck
      initialTypeEnv
      ( do
          let stmts = map topLevelStmt toplevels
          installFunctionItems stmts
          typedTopLevels <- mapM checkTopLevel toplevels
          return $ Program typedTopLevels
      )
  where
    topLevelStmt (TopLevelStmt stmt) = stmt

-- FIX: Support only value declarations in the top level.
checkTopLevel :: ParsedTopLevel -> Check TypedTopLevel
checkTopLevel (TopLevelStmt stmt)
  | isJust $ functionItem stmt = TopLevelStmt <$> checkFunctionItemStmt stmt
  | otherwise = TopLevelStmt <$> checkStmtM stmt

-- REPL

checkReplWithEnv :: TypeEnv -> ParsedRepl -> Either Error (TypedRepl, TypeEnv)
checkReplWithEnv env (Repl inputs) =
  runCheck env $ do
    installFunctionItems (mapMaybe isStmtAndFunctionItem inputs)
    typedInputs <- mapM checkReplInput inputs
    return $ Repl typedInputs
  where
    isStmtAndFunctionItem (ReplStmt s) = functionItem s
    isStmtAndFunctionItem _ = Nothing

checkRepl :: ParsedRepl -> Either Error TypedRepl
checkRepl = fmap fst . checkReplWithEnv initialTypeEnv

checkReplInput :: ParsedReplInput -> Check TypedReplInput
checkReplInput = \case
  ReplStmt stmt
    | isJust $ functionItem stmt -> ReplStmt <$> checkFunctionItemStmt stmt
    | otherwise -> ReplStmt <$> checkStmtM stmt
  ReplExpr expr -> ReplExpr <$> checkExprM expr

-- Expressions

checkExpr :: ParsedExpr -> Either Error TypedExpr
checkExpr expr = fst <$> runCheck [] (checkExprM expr)

checkExprM :: ParsedExpr -> Check TypedExpr
checkExprM (ParsedExpr pos k) = case k of
  ParsedIntLit i -> return $ TypedExpr pos (IntT Signed I32) (TypedIntLit i)
  ParsedFloatLit f -> return $ TypedExpr pos (FloatT F64) (TypedFloatLit f)
  ParsedBoolLit b -> return $ TypedExpr pos BoolT (TypedBoolLit b)
  ParsedStringLit s -> return $ TypedExpr pos StringT (TypedStringLit s)
  ParsedUnitLit -> return $ TypedExpr pos UnitT TypedUnitLit
  ParsedTypeLit ts -> TypedExpr pos TypeT . TypedTypeLit <$> checkType ts
  ParsedUnaryExpr op e -> checkUnaryExpr pos op e
  ParsedBinaryExpr op l r -> checkBinaryExpr pos op l r
  ParsedVarExpr i@(Ident _ name) -> do
    env <- getEnv
    case lookupName name env of
      Just (typ, _) -> return $ TypedExpr pos typ (TypedVarExpr i)
      Nothing -> liftEither $ Left $ UndefinedVariable pos name
  ParsedIfExpr c t e -> checkIfExpr pos c t e
  ParsedWhileExpr c b mElse -> checkWhileExpr pos c b mElse
  ParsedFnExpr params ret body -> checkFnExpr pos params ret body
  ParsedCallExpr callee args -> checkCallExpr pos callee args
  ParsedBreakExpr mExpr -> checkBreakExpr pos mExpr
  ParsedContinueExpr -> checkContinueExpr pos

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

checkWhileExpr :: AlexPosn -> ParsedExpr -> ParsedBlock -> Maybe ParsedBlock -> Check TypedExpr
checkWhileExpr pos cond body mElse = do
  tc <- checkExprM cond
  unless (isBoolLike (typeOf tc)) $ liftEither $ Left $ TypeMismatch pos (typeOf tc) BoolT

  pushLoop
  tb <- withScope (checkBlockM body)
  unless (blockType tb == UnitT) $ liftEither $ Left $ NonUnitLoopBody pos (blockType tb)

  breakTypes <- popLoop
  te <- traverse checkBlockM mElse
  ty <- case (blockType <$> te, breakTypes) of
    (Just _, []) -> liftEither $ Left $ ElseWithoutBreak pos
    (Just t, _) -> liftEither $ foldM (unifyTypes pos) t breakTypes
    (Nothing, []) -> return UnitT
    (Nothing, _ : _) -> return UnitT

  return $ TypedExpr pos ty (TypedWhileExpr tc tb te)

unifyTypes :: AlexPosn -> Type -> Type -> Either Error Type
unifyTypes pos t1 t2
  | compatible t1 t2 = Right t1
  | otherwise = Left $ TypeMismatch pos t1 t2

checkBreakExpr :: AlexPosn -> ParsedExpr -> Check TypedExpr
checkBreakExpr pos mExpr = do
  (_, ls) <- get
  when (null ls) $ liftEither $ Left $ BreakOutsideLoop pos
  te <- checkExprM mExpr
  recordBreak (Just (typeOf te))
  return $ TypedExpr pos UnitT (TypedBreakExpr te)

checkContinueExpr :: AlexPosn -> Check TypedExpr
checkContinueExpr pos = do
  (_, ls) <- get
  when (null ls) $ liftEither $ Left $ ContinueOutsideLoop pos
  return $ TypedExpr pos UnitT TypedContinueExpr

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
  outerState <- get
  let env = fst outerState
  put (pushScope env, [])
  mapM_ bindTypedParam typedParams
  typedBody <- checkBlockM body
  put outerState
  unless (compatible typedRet (blockType typedBody)) $ liftEither $ tymismatch typedRet typedBody
  return $ tyexpr (map typedParamType typedParams) typedParams typedRet typedBody
  where
    tymismatch r b = Left $ TypeMismatch pos r (blockType b)
    tyexpr ts ps r b = TypedExpr pos (FnT ts r) (TypedFnExpr ps r b)

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
    _ | isPrintf typedCallee -> checkPrintfCall pos typedCallee typedArgs
    _ | isAs typedCallee -> checkAsCall pos typedCallee typedArgs
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
    numeric = checkOpWith numericCompatible const pos op t1 t2
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
  (StringT, StringT) -> True
  (UnitT, UnitT) -> True
  (TypeT, TypeT) -> True
  (AnyTypeT, _) -> True
  (_, AnyTypeT) -> True
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
checkStmt stmt = fst <$> runCheck [] (checkStmtM stmt)

checkStmtM :: ParsedStmt -> Check TypedStmt
checkStmtM (ParsedStmt pos k) = case k of
  ParsedDeclStmt decl -> TypedStmt pos . TypedDeclStmt <$> checkDecl decl
  ParsedExprStmt expr -> TypedStmt pos . TypedExprStmt <$> checkExprM expr
  ParsedAssignStmt assign -> TypedStmt pos . TypedAssignStmt <$> checkAssign assign

checkFunctionItemStmt :: ParsedStmt -> Check TypedStmt
checkFunctionItemStmt (ParsedStmt pos (ParsedDeclStmt (ParsedValueDecl m ident mts expr@(ParsedExpr _ (ParsedFnExpr {}))))) = do
  te <- checkExprM expr
  let inferred = typeOf te
  typ <- case mts of
    Nothing -> return inferred
    Just ts -> do
      t <- checkType ts
      unless (compatible t inferred) $ liftEither $ Left $ TypeMismatch (identPos ident) t inferred
      return t
  return $ TypedStmt pos $ TypedDeclStmt $ TypedValueDecl m ident typ te
checkFunctionItemStmt stmt = checkStmtM stmt

checkDecl :: ParsedDecl -> Check TypedDecl
checkDecl (ParsedValueDecl mut ident mTs expr) = do
  mt <- traverse checkType mTs
  te <- checkExprM expr
  let inferred = typeOf te
  case mt of
    Just t | not $ compatible t inferred -> liftEither $ Left $ TypeMismatch (identPos ident) t inferred
    _ -> pure ()
  bindIdent ident inferred mut
  return $ TypedValueDecl mut ident inferred te

checkAssign :: ParsedAssign -> Check TypedAssign
checkAssign (ParsedAssign (Ident pos name) expr) = do
  env <- getEnv
  case lookupName name env of
    Nothing -> liftEither $ Left $ UndefinedVariable pos name
    Just (_, Constant) -> liftEither $ Left $ AssignToConstant pos name
    Just (varT, Mutable) -> do
      typedExpr <- checkExprM expr
      unless (compatible varT (typeOf typedExpr)) $ liftEither $ Left $ TypeMismatch pos varT (typeOf typedExpr)
      return $ TypedAssign name typedExpr

bindIdent :: Ident -> Type -> Mutability -> Check ()
bindIdent (Ident pos name) typ mut = do
  case (typ, mut) of
    (TypeT, Mutable) -> liftEither $ Left $ MutableType pos typ
    (FnT _ _, Mutable) -> liftEither $ Left $ MutableType pos typ
    _ -> return ()

  env <- getEnv
  case lookupName name env of
    Just _ -> liftEither $ Left $ DuplicateDeclaration pos name
    Nothing -> modifyEnv (bindInCurrentScope name (typ, mut))

checkParam :: Param -> Check TypedParam
checkParam (Param ident typ) = TypedParam ident <$> checkType typ

bindTypedParam :: TypedParam -> Check ()
bindTypedParam (TypedParam ident typ) = bindIdent ident typ Constant

typedParamType :: TypedParam -> Type
typedParamType (TypedParam _ typ) = typ

-- Function items

functionItem :: ParsedStmt -> Maybe ParsedStmt
functionItem i = case i of
  ParsedStmt _ (ParsedDeclStmt (ParsedValueDecl Constant _ _ (ParsedExpr _ (ParsedFnExpr {})))) -> Just i
  _ -> Nothing

installFunctionItems :: [ParsedStmt] -> Check ()
installFunctionItems stmts = do
  let fns = functionItems stmts
  checkDuplicateFunctions (fnItemsPos fns) (map (identName . fnItemIdent) fns)
  typedFns <- mapM typeFunctionItem fns
  unless (null fns) $ modifyEnv pushScope
  mapM_ (\(Ident pos name, typ) -> bindIdent (Ident pos name) typ Constant) typedFns

data FunctionItem = FunctionItem Ident [Param] TypeSyntax

fnItemIdent :: FunctionItem -> Ident
fnItemIdent (FunctionItem ident _ _) = ident

functionItems :: [ParsedStmt] -> [FunctionItem]
functionItems = mapMaybe $ \case
  ParsedStmt _ (ParsedDeclStmt (ParsedValueDecl Constant ident _ (ParsedExpr _ (ParsedFnExpr params ret _)))) ->
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
checkDuplicateFunctions _ [] = return ()
checkDuplicateFunctions pos (x : xs)
  | x `elem` xs = liftEither $ Left (DuplicateFunction pos x)
  | otherwise = checkDuplicateFunctions pos xs

-- Blocks

checkBlock :: ParsedBlock -> Either Error TypedBlock
checkBlock block = fst <$> runCheck [] (checkBlockM block)

checkBlockM :: ParsedBlock -> Check TypedBlock
checkBlockM (Block stmts expr) = do
  outerEnv <- getEnv
  installFunctionItems stmts
  fnScope <- getEnv
  typedStmts <- mapM (checkStmtInBlock fnScope) stmts
  typedExpr <- traverse checkExprM expr
  putEnv outerEnv
  return $ Block typedStmts typedExpr

checkStmtInBlock :: TypeEnv -> ParsedStmt -> Check TypedStmt
checkStmtInBlock fnScope stmt
  | isJust $ functionItem stmt = do
      saveEnv <- getEnv
      putEnv fnScope
      typed <- checkFunctionItemStmt stmt
      putEnv saveEnv
      return typed
  | otherwise = checkStmtM stmt

blockType :: TypedBlock -> Type
blockType (Block _ Nothing) = UnitT
blockType (Block _ (Just expr)) = typeOf expr
