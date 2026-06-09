{-# LANGUAGE RecordWildCards #-}
{- HLINT ignore "Use camelCase" -}
module Parser (
    parser,
    program,
    varDecl,
    mainFunc,
    subprogram,
    RootNode(..),
    showType,
    VarInfo(..),
    SubProgramInfo(..),
    ParserState(..),
    VarDecl(..),
    ParamDecl(..),
    FnDecl(..),
    FnMainDecl(..),
    StructDecl(..),
    StmtDecl(..),
    GlobalDeclType(..),
    Expr(..),
    BinOp(..),
    Primary(..),
    StructField(..),
    BaseExpression(..),
    DerivedExpression(..),
    Lit(..),
    TypeSpec(..),
    Type(..)
) where

import Lexer (Token(..), AlexPosn(..), token_posn)
import Text.Parsec
import Text.Parsec.Pos
import Control.Monad.IO.Class
import ParserHelpers.Terminals
import Data.List (intercalate)

-- AST 
data VarDecl = VarDecl
  { varId      :: String
  , varDeclType :: Maybe TypeSpec
  , varInitVal :: Maybe Expr
  } deriving (Show, Eq)

data ParamDecl = ParamDecl
  { paramMut  :: Bool -- mutabilidade
  , paramType :: Type
  , paramName :: String
  } deriving (Show, Eq)

data FnDecl = FnDecl
  { fnName      :: String
  , fnParamList :: [ParamDecl]
  , fnReturn    :: Maybe Type -- tipo de retorno opcional
  , fnBody      :: [StmtDecl]
  } deriving (Show, Eq)

data FnMainDecl = FnMainDecl
  { mainStatements :: [StmtDecl]
  } deriving (Show, Eq)

data StructDecl = StructDecl
  { structDeclName   :: String
  , structDeclFields :: [StructField]
  } deriving (Show, Eq)

data StmtDecl
    = VarStmt VarDecl
    | ExpStmt Expr
    | IfElseStmt Expr [StmtDecl] (Maybe [StmtDecl])
    | ReptStmt Expr [StmtDecl]
    | RetStmt (Maybe Expr)
    | ContinueStmt
    | BreakStmt
    deriving (Show, Eq)

data GlobalDeclType
    = GlobalVar VarDecl
    | GlobalFn FnDecl
    | GlobalStruct StructDecl
    | MainFn FnMainDecl
    deriving (Show, Eq)

data Expr
    = AssignExpr Expr Expr
    | BinaryExpr BinOp Expr Expr
    | PrimaryExpr Primary
    deriving (Show, Eq)

data BinOp
    = OpPlus | OpMinus | OpTimes | OpDiv
    | OpAnd | OpOr
    | OpEq | OpNEq | OpLt | OpGt | OpLEq | OpGEq
    deriving (Show, Eq)

data Primary = Primary
    { primaryBase     :: BaseExpression
    , primaryDerivations :: [DerivedExpression]
    } deriving (Show, Eq)

data BaseExpression
    = BaseParen Expr
    | BaseArrayInit [Lit]
    | BaseSubCall String [Expr]       -- fn_name(args)
    | BaseLiteral Lit
    | BaseId String
    deriving (Show, Eq)

data DerivedExpression
    = DerivedMember String               -- .field
    | DerivedSubCall String [Expr]       -- .method(args)
    | DerivedIndex Expr                  -- [index]
    | DerivedSlice Expr Expr             -- [start .. end]
    deriving (Show, Eq)

data Lit
    = LitInt Int
    | LitFloat Double
    | LitBool Bool
    deriving (Show, Eq)

data TypeSpec = TypeSpec
  { typeIsMut :: Bool
  , typeVal   :: Type
  } deriving (Show, Eq)

data Type
    = TyBase String      -- "u8", "i32", "bool", etc., ou tipo customizado
    | TyArray Type       -- []<type>
    deriving (Show, Eq)

-- atributos das variaveis na tabela de símbolos
data VarInfo = VarInfo
  { varScope   :: String
  , varName    :: String
  , varType    :: String
  , isMutable  :: Bool
  } deriving (Show, Eq)

-- atributos dos subprogramas na tabela de símbolos
data SubProgramInfo = SubProgramInfo
  { subName       :: String
  , subParams     :: [(String, String, Bool)] -- (nome, tipo, mutabilidade)
  , subReturnType :: Maybe String             -- Tipo de retorno opcional
  } deriving (Show, Eq)

-- atributos dos campos de um struct
data StructField = StructField
  { fieldName :: String
  , fieldType :: Type
  } deriving (Show, Eq)

-- atributos de uma struct na tabela de símbolos
data StructDefInfo = StructDefInfo
  { structName  :: String
  , structFields      :: [StructField]
  } deriving (Show, Eq)

-- estado do parser (tabela de simbolos, tabela de subprogramas e tabela de structs)
data ParserState = ParserState
  { symTable     :: [VarInfo]
  , subTable     :: [SubProgramInfo]
  , structTable  :: [StructDefInfo]
  , curScope     :: String
  } deriving (Show, Eq)

initialState :: ParserState
-- Estado vazio
initialState = ParserState [] [] [] "global"

-- Helper para converter Type em String na Tabela de Símbolos
showType :: Type -> String
showType (TyBase s)  = s
showType (TyArray t) = "[]" ++ showType t

-- Regras da gramatica. Construcao da arvore e criacao do estado

newtype RootNode = RootNode [GlobalDeclType]

-- NOTE : No codigo do prof ele tambem mantem uma lista de todos os tokens lidos. Nao sei se fez so por debug ou se 
-- eh importante para algo

-- <global_decl> → <var> ; <global_decl> | <subprogram> <global_decl> | <struct_decl> <global_decl> | <main>
global_decl :: ParsecT [Token] ParserState IO [GlobalDeclType]
global_decl =
      (do
        v <- try (varDecl <* semicolonP)
        rest <- global_decl
        return (GlobalVar v : rest)
      )
  <|> (do
        sub <- try subprogram
        rest <- global_decl
        return (GlobalFn sub : rest)
      )
  <|> (do
        s <- try structDecl
        rest <- global_decl
        return (GlobalStruct s : rest)
      )
  <|> (do
        m <- mainFunc
        eof
        return [MainFn m]
      )


program :: ParsecT [Token] ParserState IO RootNode
program = do 
  declarations <- global_decl
  return (RootNode declarations)

-- <var> → let <id> | let <id> : <type_specifier> | let <id> = <const_expr> | let <id> : <type_specifier> = <const_expr>
varDecl :: ParsecT [Token] ParserState IO VarDecl
varDecl = do
  _ <- letP
  name <- idP

  -- tipo opcional
  typeSpec <- optionMaybe (do
    _ <- colonP
    ts <- typeSpecifier
    return ts)

  -- init opcional
  initVal <- optionMaybe (do
    _ <- equalP
    e <- expr
    return e)

  -- Salva na tabela de simbolos
  st <- getState
  let scope = curScope st
      isMut = case typeSpec of
                Just ts -> typeIsMut ts
                Nothing -> False
      tName = case typeSpec of
                Just ts -> showType (typeVal ts)
                Nothing -> "unknown"

  let newVar = VarInfo scope name tName isMut
  updateState (\s -> s { symTable = symTable s ++ [newVar] })

  liftIO $ putStrLn $ "[DEBUG] Variavel '" ++ name ++ "' declarada no escopo '" ++ scope ++ "' (Mutavel: " ++ show isMut ++ ", Tipo: " ++ tName ++ ")."

  return (VarDecl name typeSpec initVal)

-- <type_specifier> → mut <type> | <type>
typeSpecifier :: ParsecT [Token] ParserState IO TypeSpec
typeSpecifier =
  (do
    _ <- mutP
    t <- typeRule
    return (TypeSpec True t)
  )
  <|> (do
    t <- typeRule
    return (TypeSpec False t)
  )

-- <type> → u8 | u16 | u32 | u64 | i8 | i16 | i32 | i64 | f32 | f64 | bool | char | []<type> | type | <id>
typeRule :: ParsecT [Token] ParserState IO Type
typeRule =
  (do
    _ <- openBrackP
    _ <- closeBrackP
    t <- typeRule
    return (TyArray t)
  )
  <|> (do
    name <- idP
    return (TyBase name)
  )

--  \<struct_decl> → struct \<id> {\<struct_fields>}
structDecl :: ParsecT [Token] ParserState IO StructDecl
structDecl = do
    _ <- structP
    sName <- idP
    _ <- openBraceP
    fList <- struct_fields
    _ <- closeBraceP

    -- salvando na tabela de structs
    let novoStruct = StructDefInfo { structName = sName, structFields = fList }
    modifyState (\st -> st { structTable = novoStruct : structTable st })
    return (StructDecl sName fList)

--  \<struct_fields> → \<struct_field>, \<struct_fields> | \<struct_field> | #sym.epsilon
struct_fields :: ParsecT [Token] ParserState IO [StructField]
struct_fields = struct_field `sepBy` commaP

--  \<struct_field> → \<id> : \<type> 
struct_field :: ParsecT [Token] ParserState IO StructField
struct_field = do
  fName        <- idP              -- Captura o nome do campo
  _            <- colonP           -- Consome o caractere ':'
  fType        <- typeRule         -- Chama a sua regra de tipos
  return (StructField { fieldName = fName, fieldType = fType })

-- <subprogram> → fn <id>(<paramt_list>){statements} | fn <id>(<paramt_list>) <type> {statements}
subprogram :: ParsecT [Token] ParserState IO FnDecl
subprogram = do
  _ <- fnP
  name <- idP

  _ <- openParP
  params <- paramt_list
  _ <- closeParP

  if name == "main" && null params
    then fail "A funcao 'main' sem parametros nao pode ser declarada como um subprograma comum."
    else return ()

  -- guarda escopo anterioro e troca para o novo
  oldState <- getState
  let oldScope = curScope oldState
  updateState (\s -> s { curScope = name })

  -- salvar parametros na tabela de simbolos
  let paramVars = map (\(ParamDecl pMut pType pName) -> VarInfo name pName (showType pType) pMut) params
  updateState (\s -> s { symTable = symTable s ++ paramVars })

  -- Tipo de retorno opcional
  retType <- optionMaybe (do
    t <- typeRule
    return t)

  -- Corpo do subprograma
  _ <- openBraceP
  bodyStmts <- statements
  _ <- closeBraceP

  -- restaurar escopo
  updateState (\s -> s { curScope = oldScope })

  -- popular a tabela de subprograma
  let paramList = map (\(ParamDecl pMut pType pName) -> (pName, showType pType, pMut)) params
      retTypeName = fmap showType retType
      newSub = SubProgramInfo name paramList retTypeName

  updateState (\s -> s { subTable = subTable s ++ [newSub] })

  -- TODO : limpar os simbolos da tabela

  liftIO $ putStrLn $ "[DEBUG] Subprograma '" ++ name ++ "' declarado."
  liftIO $ putStrLn $ "        Parametros: " ++ show paramList
  liftIO $ putStrLn $ "        Retorno: " ++ show retTypeName

  return (FnDecl name params retType bodyStmts)

-- <paramt_list> → <paramt> | <paramt>, <paramt_list> | #sym.epsilon
paramt_list :: ParsecT [Token] ParserState IO [ParamDecl]
paramt_list = paramt `sepBy` commaP

-- <paramt> → <id>: <type> | mut <id>: <type>
paramt :: ParsecT [Token] ParserState IO ParamDecl
paramt =
  (do
    _ <- mutP
    name <- idP
    _ <- colonP
    t <- typeRule
    return (ParamDecl True t name)
  )
  <|> (do
    name <- idP
    _ <- colonP
    t <- typeRule
    return (ParamDecl False t name)
  )

-- <main> → fn main(){statements}
mainFunc :: ParsecT [Token] ParserState IO FnMainDecl
mainFunc = do
  _ <- fnP
  name <- idP
  if name /= "main"
    then fail "Erro Sintatico: Esperava funcao 'main'."
    else do
      _ <- openParP
      _ <- closeParP

      -- Escopo main
      oldState <- getState
      let oldScope = curScope oldState
      updateState (\s -> s { curScope = "main" })

      _ <- openBraceP
      bodyStmts <- statements
      _ <- closeBraceP

      -- restaurar escopo
      updateState (\s -> s { curScope = oldScope })

      -- popular tabela do subprograma
      let newSub = SubProgramInfo "main" [] Nothing
      updateState (\s -> s { subTable = subTable s ++ [newSub] })

      liftIO $ putStrLn $ "[DEBUG] Subprograma 'main' declarado."

      return (FnMainDecl bodyStmts)

-- <statements> → <statement> | <statement> <statements> | #sym.epsilon
statements :: ParsecT [Token] ParserState IO [StmtDecl]
statements = (do
  first <- statement
  rest <- statements
  return (first : rest)) <|> return []

-- <statement> → <var> ; | <expr> ; | <if_else_stmt> | <repeat_stmt> | <return_stmt> ; | continue ; | break ;
statement :: ParsecT [Token] ParserState IO StmtDecl
statement =
      (try (do
        v <- varDecl
        _ <- semicolonP
        return (VarStmt v)
      ))
  <|> (try (do
        e <- expr
        _ <- semicolonP
        return (ExpStmt e)
      ))
  <|> (do
        ifs <- ifElseStmt
        return ifs
      )
  <|> (do
        rep <- repeatStmt
        return rep
      )
  <|> (do
        ret <- returnStmt
        _ <- semicolonP
        return ret
      )
  <|> (do
        _ <- continueP
        _ <- semicolonP
        return ContinueStmt
      )
  <|> (do
        _ <- breakP
        _ <- semicolonP
        return BreakStmt
      )

-- <if_else_stmt> → if (<expr>) {statements} | if(<expr>){statements} else{statements}
ifElseStmt :: ParsecT [Token] ParserState IO StmtDecl
ifElseStmt = do
  -- if
  _ <- ifP
  _ <- openParP
  cond <- expr
  _ <- closeParP

  -- then
  _ <- openBraceP
  thenBody <- statements
  _ <- closeBraceP

  -- else (se nao houver, retorna lista vaziaa)
  elsePart <- optionMaybe (do
    _ <- elseP
    _ <- openBraceP
    elseBody <- statements
    _ <- closeBraceP
    return elseBody)

  return (IfElseStmt cond thenBody elsePart)

-- <repeat_stmt> → while(<expr>){statements}
repeatStmt :: ParsecT [Token] ParserState IO StmtDecl
repeatStmt = do
  _ <- whileP
  _ <- openParP
  cond <- expr
  _ <- closeParP

  _ <- openBraceP
  body <- statements
  _ <- closeBraceP

  return (ReptStmt cond body)

-- <return_stmt> → return | return <expr>
returnStmt :: ParsecT [Token] ParserState IO StmtDecl
returnStmt = do
  _ <- returnP
  e <- optionMaybe expr
  return (RetStmt e)

expr :: ParsecT [Token] ParserState IO Expr
expr = assignExpr

-- <assign_expr> → <bool_expr> | <primary_expression> = <assign_expr>
assignExpr :: ParsecT [Token] ParserState IO Expr
assignExpr =
  (try (do
    p <- primaryExpression
    _ <- equalP
    a <- assignExpr
    return (AssignExpr (PrimaryExpr p) a)
  ))
  <|> boolExpr

-- <bool_expr> → <comparison_expr> | <comparison_expr> and <bool_expr> | <comparison_expr> or <bool_expr>
boolExpr :: ParsecT [Token] ParserState IO Expr
boolExpr = do
  c <- comparisonExpr
  rest <- optionMaybe (do
    op <- (andP >> return OpAnd) <|> (orP >> return OpOr)
    b <- boolExpr
    return (op, b))
  case rest of
    Just (op, b) -> return (BinaryExpr op c b)
    Nothing -> return c

-- <comparison_expr> → <add_expr> | <add_expr> <less_greater_equal_op> <comparison_expr>
comparisonExpr :: ParsecT [Token] ParserState IO Expr
comparisonExpr = do
  a <- addExpr
  rest <- optionMaybe (do
    symb <- compOpP
    let op = case symb of
               "==" -> OpEq
               "!=" -> OpNEq
               "<"  -> OpLt
               ">"  -> OpGt
               "<=" -> OpLEq
               ">=" -> OpGEq
               _    -> error "Op de comparacao invalido"
    c <- comparisonExpr
    return (op, c))
  case rest of
    Just (op, c) -> return (BinaryExpr op a c)
    Nothing -> return a

-- <add_expr> → <mul_expr> | <mul_expr> + <add_expr> | <mul_expr> - <add_expr>
addExpr :: ParsecT [Token] ParserState IO Expr
addExpr = do
  m <- mulExpr
  rest <- optionMaybe (do
    op <- (plusP >> return OpPlus) <|> (minusP >> return OpMinus)
    a <- addExpr
    return (op, a))
  case rest of
    Just (op, a) -> return (BinaryExpr op m a)
    Nothing -> return m

-- <mul_expr> → <primary_expr> | <primary_expr> * <mul_expr> | <primary_expr> / <mul_expr>
mulExpr :: ParsecT [Token] ParserState IO Expr
mulExpr = do
  p <- primaryExpression
  rest <- optionMaybe (do
    op <- (timesP >> return OpTimes) <|> (divP >> return OpDiv)
    a <- mulExpr
    return (op, a))
  case rest of
    Just (op, m) -> return (BinaryExpr op (PrimaryExpr p) m)
    Nothing -> return (PrimaryExpr p)

-- <primary_expression> → <id> | <member_expr> | <sub_call> | (<expr>) | <index_expr> | <slice_init> | <array_init>
primaryExpression :: ParsecT [Token] ParserState IO Primary
primaryExpression = do
  base <- baseExpression
  suffixes <- many derivedExpression -- o 'many' eh equivalente a <derived_expressions>
  return (Primary base suffixes)

-- \<base_expression> → \<id> | \<literal> | \<(expr)> | \<array_init> | \<sub_call> 
baseExpression :: ParsecT [Token] ParserState IO BaseExpression
baseExpression =
      (do
        _ <- openParP
        e <- expr
        _ <- closeParP
        return (BaseParen e)
      )
  <|> (fmap BaseArrayInit arrayInit)
  <|> try subCallBase
  <|> (fmap BaseLiteral literal)
  <|> (do
        name <- idP
        return (BaseId name)
      )

-- <derived_expression> → . \<id> | . \<sub_call> | [\<expr>] | [\<expr> .. \<expr>]
derivedExpression :: ParsecT [Token] ParserState IO DerivedExpression
derivedExpression =
      -- acesso a membros (.id) ou  sub_call
      (do
        _ <- dotP
        try (do
          subName <- idP
          _ <- openParP
          args <- argList
          _ <- closeParP
          return (DerivedSubCall subName args)
          ) <|> (do
          idName <- idP
          return (DerivedMember idName)
          )
      )
      -- ndices ou slices: [ expr ] ou [ expr ... expr ]
  <|> (do
        _ <- openBrackP
        e1 <- expr
        sliceOrIndex <- optionMaybe (do
          _ <- doubleDotP
          e2 <- expr
          return e2
          )
        _ <- closeBrackP
        case sliceOrIndex of
          Just e2 -> return (DerivedSlice e1 e2)
          Nothing -> return (DerivedIndex e1)
      )
-- \<sub_call> → \<id> (\<arg_list>)
subCallBase :: ParsecT [Token] ParserState IO BaseExpression
subCallBase = do
  name <- idP
  _ <- openParP
  args <- argList
  _ <- closeParP
  return (BaseSubCall name args)

-- \<arg_list> → \<expr> , \<arg_list> | \<expr> | #sym.Epsilon
argList :: ParsecT [Token] ParserState IO [Expr]
argList = expr `sepBy` commaP

-- <array_init> → [<literal_list>]
arrayInit :: ParsecT [Token] ParserState IO [Lit]
arrayInit = do
  _ <- openBrackP
  lits <- literalList
  _ <- closeBrackP
  return lits

literalList :: ParsecT [Token] ParserState IO [Lit]
literalList = literal `sepBy` commaP

-- <literal> → <int_literal> | <float_literal> | <bool_literal>
literal :: ParsecT [Token] ParserState IO Lit
literal =
  (do
    val <- floatLitP
    return (LitFloat val)
  )
  <|> (do
    val <- intLitP
    return (LitInt val)
  )
  <|> (do
    val <- boolLitP
    return (LitBool val)
  )

programWithState :: ParsecT [Token] ParserState IO (RootNode, ParserState)
programWithState = do
  ast <- program
  st <- getState
  return (ast, st)

parser :: [Token] -> IO (Either ParseError (RootNode, ParserState))
parser = runParserT programWithState initialState "ERRO DE PARSING!"
