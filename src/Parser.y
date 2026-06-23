{
module Parser (parseRepl, parseProgram, genAst, parseErrorInfo) where
import Lexer
import Ast.Syntax
}

%name parseRepl Repl
%name parseProgram Program
%tokentype { Token }
%error { parseError }
%monad { Alex }
%lexer { lexer } { TokenEOF }

%token
  if            { KwIf _ }
  else          { KwElse _ }
  while         { KwWhile _ }
  true          { KwTrue _ }
  false         { KwFalse _ }
  for           { KwFor _ }
  continue      { KwContinue _ }
  break         { KwBreak _ }
  fn            { KwFn _ }
  struct        { KwStruct _ }
  enum          { KwEnum _ }
  return        { KwReturn _ }
  int           { KwInt _ }
  uint          { KwUint _ }
  float         { KwFloat _ }
  bool          { KwBool _ }
  unit          { KwUnit _ }
  i8            { KwI8 _ }
  i16           { KwI16 _ }
  i32           { KwI32 _ }
  i64           { KwI64 _ }
  u8            { KwU8 _ }
  u16           { KwU16 _ }
  u32           { KwU32 _ }
  u64           { KwU64 _ }
  f32           { KwF32 _ }
  f64           { KwF64 _ }
  '='           { Equal _ }
  '<'           { Lt _ }
  '>'           { Gt _ }
  '=='          { CmpEq _ }
  '!='          { CmpNeq _ }
  '<='          { CmpLeq _ }
  '>='          { CmpGeq _ }
  '->'          { ArrowRight _ }
  '+='          { AddAssign _ }
  '-='          { SubAssign _ }
  '*='          { MulAssign _ }
  '/='          { DivAssign _ }
  and           { KwAnd _ }
  or            { KwOr _ }
  '!'           { KwNot _ }
  '+'           { Plus _ }
  '-'           { Minus _ }
  '>>'          { ShiftRight _ }
  '<<'          { ShiftLeft _ }
  '*'           { Times _ }
  '/'           { Div _ }
  '&'           { Ampersand _ }
  '('           { OpenPar _ }
  ')'           { ClosePar _ }
  '{'           { OpenBraces _ }
  '}'           { CloseBraces _ }
  '['           { OpenBrack _ }
  ']'           { CloseBrack _ }
  '::'          { DoubleDot _ }
  '.'           { Dot _ }
  ','           { Comma _ }
  ';'           { Semicolon _ }
  ':'           { Colon _ }
  "'"           { SingleQuote _ }
  '"'           { DoubleQuote _ }
  ID            { Id _ _ }
  INT_LITERAL   { IntLiteral _ _ }
  FLOAT_LITERAL { FloatLiteral _ _ }
  UnitLit       { UnitLit _ }

%%

Program : TopLevels { Program $1 }

TopLevels : TopLevel TopLevels { $1 : $2 }
          |                    { [] }

TopLevel : ValueDecl ';'  { TopLevelStmt (ParsedStmt (declPos $1) (ParsedDeclStmt $1)) }

Repl : ReplInput ';' Repl { prependInput $1 $3 }
     | ReplInput          { Repl [$1] }
     |                    { Repl [] }

ReplInput : ValueDecl  { ReplStmt (ParsedStmt (declPos $1) (ParsedDeclStmt $1)) }
          | AssignStmt { ReplStmt (ParsedStmt (assignPos $1) (ParsedAssignStmt $1)) }
          | Expr       { ReplExpr $1 }

{- statements -}

Stmt : AssignStmt ';'  { ParsedStmt (assignPos $1) (ParsedAssignStmt $1) }
     | ValueDecl ';'   { ParsedStmt (declPos $1) (ParsedDeclStmt $1) }
     | Expr ';'        { ParsedStmt (exprPos $1) (ParsedExprStmt $1) }

AssignStmt : ID '=' Expr  { ParsedAssign (toIdent $1) $3 }
           | ID '+=' Expr { ParsedAssign (toIdent $1) (ParsedExpr (token_posn $2) (ParsedBinaryExpr AddOp (ParsedExpr (token_posn $1) (ParsedVarExpr (toIdent $1))) $3)) }
           | ID '-=' Expr { ParsedAssign (toIdent $1) (ParsedExpr (token_posn $2) (ParsedBinaryExpr SubOp (ParsedExpr (token_posn $1) (ParsedVarExpr (toIdent $1))) $3)) }
           | ID '*=' Expr { ParsedAssign (toIdent $1) (ParsedExpr (token_posn $2) (ParsedBinaryExpr MulOp (ParsedExpr (token_posn $1) (ParsedVarExpr (toIdent $1))) $3)) }
           | ID '/=' Expr { ParsedAssign (toIdent $1) (ParsedExpr (token_posn $2) (ParsedBinaryExpr DivOp (ParsedExpr (token_posn $1) (ParsedVarExpr (toIdent $1))) $3)) }

ValueDecl : ID ':' Type               { ParsedValueDecl Mutable (toIdent $1) (Just $3) Nothing }
          | ID ':' OptType '=' Expr   { ParsedValueDecl Mutable (toIdent $1) $3 (Just $5) }
          | ID ':' OptType ':' Expr   { ParsedValueDecl Constant (toIdent $1) $3 (Just $5) }

{- expressions -}

Block : '{' '}'           { Block [] Nothing }
      | '{' BlockBody '}' { $2 }

BlockBody : Expr           { Block [] (Just $1) }
          | Stmt           { Block [$1] Nothing }
          | Stmt BlockBody { prependStmt $1 $2 }

Expr : IfExpr        { $1 }
     | WhileExpr     { $1 }
     | BreakExpr     { $1 }
     | ContinueExpr  { $1 }
     | FnExpr        { $1 }
     | LogicalOrExpr { $1 }

IfExpr : if Expr Block            { ParsedExpr (token_posn $1) (ParsedIfExpr $2 $3 Nothing) }
       | if Expr Block else Block { ParsedExpr (token_posn $1) (ParsedIfExpr $2 $3 (Just $5)) }

WhileExpr : while Expr Block            { ParsedExpr (token_posn $1) (ParsedWhileExpr $2 $3 Nothing) }
          | while Expr Block else Block { ParsedExpr (token_posn $1) (ParsedWhileExpr $2 $3 (Just $5)) }

BreakExpr : break        { ParsedExpr (token_posn $1) (ParsedBreakExpr (ParsedExpr (token_posn $1) ParsedUnitLit)) }
          | break Expr   { ParsedExpr (token_posn $1) (ParsedBreakExpr $2) }

ContinueExpr : continue  { ParsedExpr (token_posn $1) ParsedContinueExpr }

FnExpr : fn '(' FnParams ')' '->' Type Block { ParsedExpr (token_posn $1) (ParsedFnExpr $3 $6 $7) }
       | fn '(' FnParams ')' Block { ParsedExpr (token_posn $1) (ParsedFnExpr $3 (TypeSyntax (token_posn $1) UnitSyntax) $5) }
       | fn UnitLit '->' Type Block { ParsedExpr (token_posn $1) (ParsedFnExpr [] $4 $5) }
       | fn UnitLit Block { ParsedExpr (token_posn $1) (ParsedFnExpr [] (TypeSyntax (token_posn $1) UnitSyntax) $3) }

FnParams : FnParamList { $1 }
         |             { [] }

FnParamList : FnParam                 { [$1] }
            | FnParam ',' FnParamList { $1 : $3 }

FnParam : ID ':' Type { Param (toIdent $1) $3 }

LogicalOrExpr : LogicalOrExpr or LogicalAndExpr { ParsedExpr (token_posn $2) (ParsedBinaryExpr OrOp $1 $3) }
              | LogicalAndExpr                  { $1 }

LogicalAndExpr : LogicalAndExpr and ComparisonExpr { ParsedExpr (token_posn $2) (ParsedBinaryExpr AndOp $1 $3) }
               | ComparisonExpr                    { $1 }

ComparisonExpr : AddExpr CompareOp AddExpr { let (pos, op) = $2 in ParsedExpr pos (ParsedBinaryExpr op $1 $3) }
               | AddExpr                   { $1 }

CompareOp : '==' { (token_posn $1, EqOp) }
          | '!=' { (token_posn $1, NeqOp) }
          | '<'  { (token_posn $1, LtOp) }
          | '<=' { (token_posn $1, LeqOp) }
          | '>'  { (token_posn $1, GtOp) }
          | '>=' { (token_posn $1, GeqOp) }

AddExpr : AddExpr '+' MulExpr { ParsedExpr (token_posn $2) (ParsedBinaryExpr AddOp $1 $3) }
        | AddExpr '-' MulExpr { ParsedExpr (token_posn $2) (ParsedBinaryExpr SubOp $1 $3) }
        | MulExpr             { $1 }

MulExpr : MulExpr '*' UnaryExpr { ParsedExpr (token_posn $2) (ParsedBinaryExpr MulOp $1 $3) }
        | MulExpr '/' UnaryExpr { ParsedExpr (token_posn $2) (ParsedBinaryExpr DivOp $1 $3) }
        | UnaryExpr             { $1 }

UnaryExpr : '-' UnaryExpr { ParsedExpr (token_posn $1) (ParsedUnaryExpr NegOp $2) }
          | '&' UnaryExpr { ParsedExpr (token_posn $1) (ParsedUnaryExpr AmpersandOp $2) }
          | '!' UnaryExpr { ParsedExpr (token_posn $1) (ParsedUnaryExpr NotOp $2) }
          | CallExpr      { $1 }

CallExpr : AtomExpr              { $1 }
         | CallExpr '(' Args ')' { ParsedExpr (exprPos $1) (ParsedCallExpr $1 $3) }
         | CallExpr UnitLit      { ParsedExpr (exprPos $1) (ParsedCallExpr $1 []) }

Args : ArgList { $1 }
     |         { [] }

ArgList : Expr             { [$1] }
        | Expr ',' ArgList { $1 : $3 }

AtomExpr : INT_LITERAL   { toIntLit $1 }
         | FLOAT_LITERAL { toFloatLit $1 }
         | true          { ParsedExpr (token_posn $1) (ParsedBoolLit True) }
         | false         { ParsedExpr (token_posn $1) (ParsedBoolLit False) }
         | '(' Expr ')'  { $2 }
         | UnitLit       { ParsedExpr (token_posn $1) ParsedUnitLit }
         | ID            { ParsedExpr (token_posn $1) (ParsedVarExpr (toIdent $1)) }

{- types -}

OptType : Type { Just $1 }
        |      { Nothing }

Type : ID       { TypeSyntax (token_posn $1) (NameSyntax (idToString $1)) }
     | FnType   { $1 }
     | TypeLit  { $1 }

FnType : fn '(' FnParamsTypes ')' '->' Type { TypeSyntax (token_posn $1) (FnSyntax $3 $6) }

FnParamsTypes : FnParamsTypesList { $1 }
              |                   { [] }

FnParamsTypesList : Type                       { [$1] }
                  | Type ',' FnParamsTypesList { $1 : $3 }

TypeLit : unit  { TypeSyntax (token_posn $1) UnitSyntax }
        | bool  { TypeSyntax (token_posn $1) BoolSyntax }
        | int   { TypeSyntax (token_posn $1) (IntSyntax Signed I32) }
        | uint  { TypeSyntax (token_posn $1) (IntSyntax Unsigned I32) }
        | float { TypeSyntax (token_posn $1) (FloatSyntax F64) }
        | i8    { TypeSyntax (token_posn $1) (IntSyntax Signed I8) }
        | i16   { TypeSyntax (token_posn $1) (IntSyntax Signed I16) }
        | i32   { TypeSyntax (token_posn $1) (IntSyntax Signed I32) }
        | i64   { TypeSyntax (token_posn $1) (IntSyntax Signed I64) }
        | u8    { TypeSyntax (token_posn $1) (IntSyntax Unsigned I8) }
        | u16   { TypeSyntax (token_posn $1) (IntSyntax Unsigned I16) }
        | u32   { TypeSyntax (token_posn $1) (IntSyntax Unsigned I32) }
        | u64   { TypeSyntax (token_posn $1) (IntSyntax Unsigned I64) }
        | f32   { TypeSyntax (token_posn $1) (FloatSyntax F32) }
        | f64   { TypeSyntax (token_posn $1) (FloatSyntax F64) }

{
parseError :: Token -> Alex a
parseError tok =
  let pos = token_posn tok
  in alexError $ prettyPosn pos ++ ": unexpected " ++ prettyToken tok

parseErrorInfo :: String -> (Maybe AlexPosn, String)
parseErrorInfo s = case break (== ':') s of
  (lineStr, ':' : rest) -> case break (== ':') rest of
    (colStr, ':' : ' ' : msg) ->
      (Just (AlexPn 0 (read lineStr) (read colStr)), msg)
    _ -> (Nothing, s)
  _ -> (Nothing, s)

idToString :: Token -> String
idToString (Id pos name) = name
idToString _ = error "internal parser error: expected identifier"

toIdent :: Token -> Ident
toIdent (Id pos name) = Ident pos name
toIdent _ = error "internal parser error: expected identifier"

toIntLit :: Token -> ParsedExpr
toIntLit (IntLiteral pos value) = ParsedExpr pos (ParsedIntLit value)
toIntLit _ = error "internal parser error: expected integer literal"

toFloatLit :: Token -> ParsedExpr
toFloatLit (FloatLiteral pos value) = ParsedExpr pos (ParsedFloatLit value)
toFloatLit _ = error "internal parser error: expected float literal"

assignPos :: ParsedAssign -> AlexPosn
assignPos (ParsedAssign (Ident pos _) _) = pos

declPos :: ParsedDecl -> AlexPosn
declPos (ParsedValueDecl _ (Ident pos _) _ _) = pos

exprPos :: ParsedExpr -> AlexPosn
exprPos (ParsedExpr pos _) = pos

prependStmt :: ParsedStmt -> ParsedBlock -> ParsedBlock
prependStmt stmt (Block stmts expr) = Block (stmt : stmts) expr

prependInput :: ReplInput ParsedStmt ParsedExpr -> Repl ParsedStmt ParsedExpr -> Repl ParsedStmt ParsedExpr
prependInput i (Repl is) = Repl (i : is)

genAst = runAlex
}
