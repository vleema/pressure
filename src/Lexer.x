{
  module Lexer (tokenize, Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}

%wrapper "posn"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters

tokens :-

  $white+                         ;
  "--".*.                         ;
  let                           { \p s -> TokenLet p }
  mut                           { \p s -> TokenMut p }
  if                            { \p s -> TokenIf p }
  else                          { \p s -> TokenElse p }
  match                         { \p s -> TokenMatch p }
  true                          { \p s -> TokenTrue p }
  false                         { \p s -> TokenFalse p }
  for                           { \p s -> TokenFor p }
  while                         { \p s -> TokenWhile p }
  continue                      { \p s -> TokenContinue p }
  break                         { \p s -> TokenBreak p }
  struct                        { \p s -> TokenStruct p }
  fn                            { \p s -> TokenFn p }
  pub                           { \p s -> TokenPub p }
  enum                          { \p s -> TokenEnum p }
  return                       { \p s -> TokenReturn p }
  is                            { \p s -> TokenIs p }
  =                           { \p s -> Equal p }
  \=\= | \<\= | \>\= | \!\=     { \p s -> TokenCompSymb p s }
  [\<\>]                        { \p s -> TokenCompSymb p s }
  and                         { \p s -> TokenAnd p }
  or                          { \p s -> TokenOr p }
  not                         { \p s -> TokenNot p }
  \+                            { \p s -> Plus p }
  \-                            { \p s -> Minus p }
  \>\>                          { \p s -> ShiftRight p }
  \<\<                          { \p s -> ShiftLeft p }
  \*                            { \p s -> Times p }
  \/                            { \p s -> Div p }
  \&                            { \p s -> BitAnd p }
  \|                            { \p s -> BitOr p }
  \~                            { \p s -> BitNot p }
  \(			                    { \p s -> OpenPar p }
  \)			                    { \p s -> ClosePar p }
  \{			                    { \p s -> OpenBraces p }
  \}			                    { \p s -> CloseBraces p }
  \[			                    { \p s -> OpenBrack p }
  \]			                    { \p s -> CloseBrack p }
  \.                           { \p s -> Dot p }
  \,                           { \p s -> Comma p }
  \;                            { \p s -> Semicolon p }
  \:                           { \p s -> Colon p }
  \.\.                          { \p s -> DoubleDot p}
  \'                            {\p s -> SingleQuote p}
  \"                            {\p s -> DoubleQuote p}
  $alpha [$alpha $digit \_ \']*	  { \p s -> Id p s }
  $digit+                    { \p s -> IntLiteral p (read s) }

{
-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:

-- The token type:
data Token =
  Let AlexPosn        |
  IntLiteral AlexPosn Int    |
  OpenPar AlexPosn   |
  ClosePar AlexPosn  |
  OpenBraces AlexPosn |
  CloseBraces AlexPosn |
  OpenBrack AlexPosn  |
  CloseBrack AlexPosn |
  TokenLet AlexPosn     |
  TokenMut AlexPosn     |
  TokenIf AlexPosn      | 
  TokenElse AlexPosn    |
  TokenMatch AlexPosn   |
  TokenTrue AlexPosn    |
  TokenFalse AlexPosn   |
  TokenFor AlexPosn     |
  TokenWhile AlexPosn   |
  TokenContinue AlexPosn|
  TokenBreak AlexPosn   |
  TokenStruct AlexPosn  |
  TokenFn AlexPosn      |
  TokenPub AlexPosn     |
  TokenEnum AlexPosn    |
  TokenReturn AlexPosn  |
  TokenIs AlexPosn      |
  TokenAnd AlexPosn     |
  TokenOr AlexPosn      |
  TokenCompSymb AlexPosn String |
  Minus AlexPosn        |
  Plus AlexPosn         |
  Times AlexPosn        |
  Div AlexPosn          |
  BitAnd AlexPosn       |
  BitOr AlexPosn        |
  BitNot AlexPosn       |
  ShiftRight AlexPosn   |
  ShiftLeft AlexPosn    |
  TokenNot AlexPosn     |
  Equal AlexPosn        |
  Id AlexPosn String    |
  Int AlexPosn Int       |
  Colon AlexPosn        |
  Comma AlexPosn        |
  Semicolon AlexPosn    |
  Dot AlexPosn          |
  SingleQuote AlexPosn |
  DoubleQuote AlexPosn |
  DoubleDot AlexPosn
  deriving (Eq,Show)

token_posn :: Token -> AlexPosn
token_posn (TokenLet p)     = p
token_posn (TokenMut p)     = p
token_posn (TokenIf p)      = p
token_posn (TokenElse p)    = p
token_posn (TokenMatch p)   = p
token_posn (TokenTrue p)    = p
token_posn (TokenFalse p)   = p
token_posn (TokenFor p)     = p
token_posn (TokenWhile p)   = p
token_posn (TokenContinue p)= p
token_posn (TokenBreak p)   = p
token_posn (TokenStruct p)  = p
token_posn (TokenFn p)      = p
token_posn (TokenPub p)     = p
token_posn (TokenEnum p)    = p
token_posn (TokenIs p)      = p
token_posn (IntLiteral p _)   = p
token_posn (OpenPar p)      = p
token_posn (ClosePar p)     = p
token_posn (OpenBraces p)   = p
token_posn (CloseBraces p)  = p
token_posn (OpenBrack p)    = p
token_posn (CloseBrack p)   = p
token_posn (TokenAnd p)     = p
token_posn (TokenOr p)      = p
token_posn (TokenCompSymb p _) = p
token_posn (Minus p)        = p
token_posn (Plus p)         = p
token_posn (Times p)        = p
token_posn (Div p)          = p
token_posn (BitAnd p)       = p
token_posn (BitOr p)        = p
token_posn (BitNot p)       = p
token_posn (ShiftRight p)   = p
token_posn (ShiftLeft p)    = p
token_posn (TokenNot p)     = p
token_posn (Equal p)        = p
token_posn (Id p _)        = p
token_posn (Colon p)        = p
token_posn (Comma p)        = p
token_posn (Semicolon p)    = p
token_posn (Dot p)          = p
token_posn (DoubleDot p)  = p
token_posn (TokenReturn p)    = p
token_posn (SingleQuote p)  = p
token_posn (DoubleQuote p)  = p


tokenize :: String -> [Token]
tokenize = alexScanTokens
}