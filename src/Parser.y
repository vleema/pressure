{
module Parser where
import Lexer
import Ast.Syntax
}

%name parseRepl ReplInput
%name parseProgram Program
%tokentype { Token }
%error { parseError }
%monad { Alex }
%lexer { lexer } { TokenEOF }

%token
  if            { KwIf _ }
  else          { KwElse _ }
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

%%

Program : TopLevels { Program $1 }

TopLevels : TopLevel TopLevels { $1 : $2 }
          |                    { [] }

TopLevel : Stmt   { TopLevelStmt $1 }
         | IfExpr { TopLevelStmt (Stmt (exprPos $1) (ExprStmt $1)) }

ReplInput : Stmt      { ReplStmt $1 }
          | Expr      { ReplExpr $1 }

{- statements -}

Stmt : ValueDecl ';' { Stmt (declPos $1) (DeclStmt $1) }
     | Expr ';'      { Stmt (exprPos $1) (ExprStmt $1) }

ValueDecl : ID ':' Type               { ValueDecl Mutable (toIdent $1) (Just $3) Nothing }
          | ID ':' OptType '=' Expr   { ValueDecl Mutable (toIdent $1) $3 (Just $5) }
          | ID ':' OptType ':' Expr   { ValueDecl Constant (toIdent $1) $3 (Just $5) }

{- expressions -}

Block : '{' '}'           { Block [] Nothing }
      | '{' BlockBody '}' { $2 }

BlockBody : Expr           { Block [] (Just $1) }
          | Stmt           { Block [$1] Nothing }
          | Stmt BlockBody { prependStmt $1 $2 }

Expr : IfExpr        { $1 }
     | FnExpr        { $1 }
     | LogicalOrExpr { $1 }

IfExpr : if Expr Block            { Expr (token_posn $1) (IfExpr $2 $3 Nothing) }
       | if Expr Block else Block { Expr (token_posn $1) (IfExpr $2 $3 (Just $5)) }

FnExpr : fn '(' FnParams ')' '->' Type Block { Expr (token_posn $1) (FnExpr $3 $6 $7) }

FnParams : FnParamList { $1 }
         |             { [] }

FnParamList : FnParam                 { [$1] }
            | FnParam ',' FnParamList { $1 : $3 }

FnParam : ID ':' Type { Param (toIdent $1) $3 }

LogicalOrExpr : LogicalOrExpr or LogicalAndExpr { Expr (token_posn $2) (BinaryExpr OrOp $1 $3) }
              | LogicalAndExpr                  { $1 }

LogicalAndExpr : LogicalAndExpr and ComparisonExpr { Expr (token_posn $2) (BinaryExpr AndOp $1 $3) }
               | ComparisonExpr                    { $1 }

ComparisonExpr : AddExpr CompareOp AddExpr { let (pos, op) = $2 in Expr pos (BinaryExpr op $1 $3) }
               | AddExpr                   { $1 }

CompareOp : '==' { (token_posn $1, EqOp) }
          | '!=' { (token_posn $1, NeqOp) }
          | '<'  { (token_posn $1, LtOp) }
          | '<=' { (token_posn $1, LeqOp) }
          | '>'  { (token_posn $1, GtOp) }
          | '>=' { (token_posn $1, GeqOp) }

AddExpr : AddExpr '+' MulExpr { Expr (token_posn $2) (BinaryExpr AddOp $1 $3) }
        | AddExpr '-' MulExpr { Expr (token_posn $2) (BinaryExpr SubOp $1 $3) }
        | MulExpr             { $1 }

MulExpr : MulExpr '*' UnaryExpr { Expr (token_posn $2) (BinaryExpr MulOp $1 $3) }
        | MulExpr '/' UnaryExpr { Expr (token_posn $2) (BinaryExpr DivOp $1 $3) }
        | UnaryExpr             { $1 }

UnaryExpr : '-' UnaryExpr { Expr (token_posn $1) (UnaryExpr NegOp $2) }
          | '&' UnaryExpr { Expr (token_posn $1) (UnaryExpr AmpersandOp $2) }
          | '!' UnaryExpr { Expr (token_posn $1) (UnaryExpr NotOp $2) }
          | CallExpr      { $1 }

CallExpr : AtomExpr              { $1 }
         | CallExpr '(' Args ')' { Expr (exprPos $1) (CallExpr $1 $3) }

Args : ArgList { $1 }
     |         { [] }

ArgList : Expr             { [$1] }
        | Expr ',' ArgList { $1 : $3 }

AtomExpr : INT_LITERAL   { toIntLit $1 }
         | FLOAT_LITERAL { toFloatLit $1 }
         | true          { Expr (token_posn $1) (BoolLit True) }
         | false         { Expr (token_posn $1) (BoolLit False) }
         | '(' Expr ')'  { $2 }
         | ID            { Expr (token_posn $1) (VarExpr (toIdent $1)) }

{- types -}

OptType : Type { Just $1 }
        |      { Nothing }

Type : ID       { TypeName (toIdent $1) }
     | FnType   { $1 }
     | TypeLit  { $1 }

FnType : fn '(' FnParamsTypes ')' '->' Type { FnType (token_posn $1) $3 $6 }

FnParamsTypes : FnParamsTypesList { $1 }
              |                   { [] }

FnParamsTypesList : Type                       { [$1] }
                  | Type ',' FnParamsTypesList { $1 : $3 }

TypeLit : bool  { BoolType (token_posn $1) }
        | int   { IntType (token_posn $1) Signed I32 }
        | uint  { IntType (token_posn $1) Unsigned I32 }
        | float { FloatType (token_posn $1) F64 }
        | i8    { IntType (token_posn $1) Signed I8 }
        | i16   { IntType (token_posn $1) Signed I16 }
        | i32   { IntType (token_posn $1) Signed I32 }
        | i64   { IntType (token_posn $1) Signed I64 }
        | u8    { IntType (token_posn $1) Unsigned I8 }
        | u16   { IntType (token_posn $1) Unsigned I16 }
        | u32   { IntType (token_posn $1) Unsigned I32 }
        | u64   { IntType (token_posn $1) Unsigned I64 }
        | f32   { FloatType (token_posn $1) F32 }
        | f64   { FloatType (token_posn $1) F64 }

{
parseError :: Token -> Alex a
parseError tok =
  let AlexPn _ line col = token_posn tok
  in alexError $ "at " ++ show line ++ ":" ++ show col
              ++ ": unexpected " ++ show tok

toIdent :: Token -> Ident
toIdent (Id pos name) = Ident pos name
toIdent _ = error "internal parser error: expected identifier"

toIntLit :: Token -> ParsedExpr
toIntLit (IntLiteral pos value) = Expr pos (IntLit value)
toIntLit _ = error "internal parser error: expected integer literal"

toFloatLit :: Token -> ParsedExpr
toFloatLit (FloatLiteral pos value) = Expr pos (FloatLit value)
toFloatLit _ = error "internal parser error: expected float literal"

declPos :: ParsedDecl -> AlexPosn
declPos (ValueDecl _ (Ident pos _) _ _) = pos

exprPos :: ParsedExpr -> AlexPosn
exprPos (Expr pos _) = pos

prependStmt :: ParsedStmt -> ParsedBlock -> ParsedBlock
prependStmt stmt (Block stmts expr) = Block (stmt : stmts) expr

genAst = runAlex
}
