{-# LANGUAGE RecordWildCards #-}
module ParserHelpers.PrettyPrint() where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)

-- 1. Import all the AST types from your Parser module
import Parser
    ( RootNode(..)
    , GlobalDeclType(..)
    , VarDecl(..)
    , FnDecl(..)
    , StructDecl(..)
    , FnMainDecl(..)
    , StmtDecl(..)
    , Expr(..)
    , BinOp(..)
    , Primary(..)
    , BaseExpression(..)
    , DerivedExpression(..)
    , Lit(..)
    , TypeSpec(..)
    , Type(..)
    , StructField(..)
    , showType, ParamDecl (..)
    )

-- 2. Paste your exact 'instance Show RootNode' block here
instance Show RootNode where
    show (RootNode globals) = "RootNode\n" ++ showGlobalList 1 [False] globals
      where
        showGlobalList :: Int -> [Bool] -> [GlobalDeclType] -> String
        showGlobalList _ _ [] = ""
        showGlobalList depth isLastList [g]    = showGlobal depth (isLastList ++ [True]) g
        showGlobalList depth isLastList (g:gs) = showGlobal depth (isLastList ++ [False]) g ++ showGlobalList depth isLastList gs

        showGlobal :: Int -> [Bool] -> GlobalDeclType -> String
        showGlobal depth isLast decl = case decl of
            GlobalVar var -> 
                makePrefix isLast ++ "├─ GlobalVar\n" ++ showVar depth (isLast ++ [True]) var
            GlobalFn fn -> 
                makePrefix isLast ++ "├─ GlobalFn\n" ++ showFn depth (isLast ++ [True]) fn
            GlobalStruct struct -> 
                makePrefix isLast ++ "├─ GlobalStruct\n" ++ showStruct depth (isLast ++ [True]) struct
            MainFn mainFn -> 
                makePrefix isLast ++ "├─ MainFn\n" ++ showMain depth (isLast ++ [True]) mainFn

        showVar :: Int -> [Bool] -> VarDecl -> String
        showVar depth isLast VarDecl{..} =
            unlines [ makePrefix isLast ++ "└─ VarDecl"
                    , makePrefix (isLast ++ [False]) ++ "├─ Name: " ++ varId
                    , makePrefix (isLast ++ [False]) ++ "├─ Type: " ++ maybe "Infer" showTypeSpec varDeclType
                    , makePrefix (isLast ++ [True])  ++ "└─ Init:\n" ++ maybe (makePrefix (isLast ++ [True] ++ [True]) ++ "None") (showExpr (depth + 2) (isLast ++ [True, True])) varInitVal
                    ]

        showTypeSpec :: TypeSpec -> String
        showTypeSpec TypeSpec{..} = (if typeIsMut then "mut " else "") ++ showType typeVal

        showStruct :: Int -> [Bool] -> StructDecl -> String
        showStruct depth isLast StructDecl{..} =
            makePrefix isLast ++ "└─ StructDecl\n" ++
            makePrefix (isLast ++ [False]) ++ "├─ Name: " ++ structDeclName ++ "\n" ++
            makePrefix (isLast ++ [True])  ++ "└─ Fields\n" ++ showFields (depth + 2) (isLast ++ [True, True]) structDeclFields
          where
            showFields _ _ [] = ""
            showFields d isL [f]    = showField d (isL ++ [True]) f
            showFields d isL (f:fs) = showField d (isL ++ [False]) f ++ showFields d isL fs
            showField d isL StructField{..} = makePrefix isL ++ "└─ " ++ fieldName ++ ": " ++ showType fieldType ++ "\n"

        showFn :: Int -> [Bool] -> FnDecl -> String
        showFn depth isLast FnDecl{..} =
            makePrefix isLast ++ "└─ FnDecl\n" ++
            makePrefix (isLast ++ [False]) ++ "├─ Name: " ++ fnName ++ "\n" ++
            makePrefix (isLast ++ [False]) ++ "├─ Return Type: " ++ maybe "void" showType fnReturn ++ "\n" ++
            makePrefix (isLast ++ [False]) ++ "├─ Parameters\n" ++ showParams (depth + 2) (isLast ++ [False, True]) fnParamList ++
            makePrefix (isLast ++ [True])  ++ "└─ Body\n" ++ showStmts (depth + 2) (isLast ++ [True, True]) fnBody
          where
            showParams _ _ [] = ""
            showParams d isL [p]    = showParam d (isL ++ [True]) p
            showParams d isL (p:ps) = showParam d (isL ++ [False]) p ++ showParams d isL ps
            showParam d isL ParamDecl{..} = makePrefix isL ++ "└─ Param: " ++ paramName ++ " (" ++ (if paramMut then "mut " else "") ++ showType paramType ++ ")\n"

        showMain :: Int -> [Bool] -> FnMainDecl -> String
        showMain depth isLast FnMainDecl{..} =
            makePrefix isLast ++ "└─ FnMainDecl\n" ++
            makePrefix (isLast ++ [True]) ++ "└─ Body\n" ++ showStmts (depth + 2) (isLast ++ [True, True]) mainStatements

        showStmts :: Int -> [Bool] -> [StmtDecl] -> String
        showStmts _ _ [] = ""
        showStmts d isLast [s]    = showStmt d (isLast ++ [True]) s
        showStmts d isLast (s:ss) = showStmt d (isLast ++ [False]) s ++ showStmts d isLast ss

        showStmt :: Int -> [Bool] -> StmtDecl -> String
        showStmt depth isLast stmt = case stmt of
            VarStmt var -> makePrefix isLast ++ "├─ VarStmt\n" ++ showVar (depth + 1) (isLast ++ [True]) var
            ExpStmt exprTree -> makePrefix isLast ++ "├─ ExpStmt\n" ++ showExpr (depth + 1) (isLast ++ [True]) exprTree
            ContinueStmt -> makePrefix isLast ++ "└─ Continue\n"
            BreakStmt -> makePrefix isLast ++ "└─ Break\n"
            RetStmt value -> makePrefix isLast ++ "├─ ReturnStmt\n" ++ maybe (makePrefix (isLast ++ [True]) ++ "└─ void\n") (showExpr (depth + 1) (isLast ++ [True])) value
            ReptStmt cond body ->
                makePrefix isLast ++ "├─ ReptStmt (While)\n" ++
                makePrefix (isLast ++ [False]) ++ "├─ Condition\n" ++ showExpr (depth + 2) (isLast ++ [False, True]) cond ++
                makePrefix (isLast ++ [True])  ++ "└─ Body\n" ++ showStmts (depth + 2) (isLast ++ [True, True]) body
            IfElseStmt cond thenB elseB ->
                makePrefix isLast ++ "├─ IfElseStmt\n" ++
                makePrefix (isLast ++ [False]) ++ "├─ Condition\n" ++ showExpr (depth + 2) (isLast ++ [False, True]) cond ++
                makePrefix (isLast ++ [False]) ++ "├─ Then\n" ++ showStmts (depth + 2) (isLast ++ [False, True]) thenB ++
                makePrefix (isLast ++ [True])  ++ "└─ Else\n" ++ maybe (makePrefix (isLast ++ [True] ++ [True]) ++ "└─ None\n") (showStmts (depth + 2) (isLast ++ [True, True])) elseB

        showExpr :: Int -> [Bool] -> Expr -> String
        showExpr depth isLast expression = case expression of
            AssignExpr lhs rhs -> makePrefix isLast ++ "├─ Assign\n" ++ showExpr (depth + 1) (isLast ++ [False]) lhs ++ showExpr (depth + 1) (isLast ++ [True]) rhs
            BinaryExpr op left right -> makePrefix isLast ++ "├─ BinaryOp (" ++ showOp op ++ ")\n" ++ showExpr (depth + 1) (isLast ++ [False]) left ++ showExpr (depth + 1) (isLast ++ [True]) right
            PrimaryExpr prim -> showPrimary depth isLast prim

        showOp :: BinOp -> String
        showOp op = case op of
            OpPlus -> "+" ; OpMinus -> "-" ; OpTimes -> "*" ; OpDiv -> "/"
            OpAnd  -> "&&"; OpOr    -> "||"
            OpEq   -> "=="; OpNEq   -> "!="; OpLt    -> "<" ; OpGt  -> ">"; OpLEq -> "<="; OpGEq -> ">="

        showPrimary :: Int -> [Bool] -> Primary -> String
        showPrimary depth isLast Primary{..} = case primaryDerivations of
            [] -> showBase depth isLast primaryBase
            derivs -> makePrefix isLast ++ "├─ Primary\n" ++ showBase (depth + 1) (isLast ++ [False]) primaryBase ++ makePrefix (isLast ++ [True]) ++ "└─ Suffixes\n" ++ showDerivs (depth + 2) (isLast ++ [True, True]) derivs
          where
            showDerivs _ _ [] = ""
            showDerivs d isL [x]    = showDeriv d (isL ++ [True]) x
            showDerivs d isL (x:xs) = showDeriv d (isL ++ [False]) x ++ showDerivs d isL xs
            showDeriv d isL dExp = case dExp of
                DerivedMember m   -> makePrefix isL ++ "└─ Access Member: ." ++ m ++ "\n"
                DerivedIndex idx  -> makePrefix isL ++ "├─ Index At\n" ++ showExpr (d + 1) (isL ++ [True]) idx
                DerivedSlice s e  -> makePrefix isL ++ "├─ Slice Range\n" ++ showExpr (d + 1) (isL ++ [False]) s ++ showExpr (d + 1) (isL ++ [True]) e
                DerivedSubCall n args -> makePrefix isL ++ "├─ Method Call: ." ++ n ++ "\n" ++ makePrefix (isL ++ [True]) ++ "└─ Args\n" ++ concatMap (showExpr (d + 2) (isL ++ [True, True])) args

        showBase :: Int -> [Bool] -> BaseExpression -> String
        showBase depth isLast bExp = case bExp of
            BaseId identifier  -> makePrefix isLast ++ "└─ Id: " ++ identifier ++ "\n"
            BaseLiteral lit    -> makePrefix isLast ++ "└─ Literal: " ++ showLit lit ++ "\n"
            BaseParen inner    -> makePrefix isLast ++ "├─ Parenthesized\n" ++ showExpr (depth + 1) (isLast ++ [True]) inner
            BaseArrayInit lits -> makePrefix isLast ++ "└─ ArrayInit: [" ++ interimLits lits ++ "]\n"
            BaseSubCall name args -> makePrefix isLast ++ "├─ Free Function Call: " ++ name ++ "\n" ++ makePrefix (isLast ++ [True]) ++ "└─ Args\n" ++ concatMap (showExpr (depth + 2) (isLast ++ [True, True])) args
          where interimLits = intercalate ", " . map showLit

        showLit :: Lit -> String
        showLit (LitInt i)   = show i
        showLit (LitFloat f) = show f
        showLit (LitBool b)  = show b

        makePrefix :: [Bool] -> String
        makePrefix []  = ""
        makePrefix [_] = ""
        makePrefix list = concatMap (\isL -> if isL then "    " else "│   ") (init list)