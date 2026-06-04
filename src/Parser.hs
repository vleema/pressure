{- HLINT ignore "Use newtype instead of data" -}
module Parser () where
import Lexer (Token(..), AlexPosn(..))
import Text.Parsec

-- parsers para os terminais
--declaração da variavel vai ter nome, possivelmente tipo e possivelmente valor inicial
data VarDecl = VarDecl{
        varId :: String,
        varType :: Maybe String,
        varInitVal :: Maybe Int
} deriving (Show, Eq)
data ParamDecl = ParamDecl{
    paramType :: String,
    paramName :: String
} deriving (Show, Eq)

data FnDecl = FnDecl{
    fnName :: String,
    fnParamList :: [ParamDecl],
    fnReturn :: String,
    fnBody :: [Token]
} deriving (Show, Eq)

data FnMainDecl = FnMainDecl{
    mainStatements :: [Token]
} deriving (Show, Eq)

data GlobalDeclType 
    = GlobalVar VarDecl
    | GlobalFn FnDecl
    | MainFn 
    deriving (Show, Eq)

parseGlobalDecl :: Parsec [Token] st [GlobalDeclType]
parseGlobalDecl = do
    global_declarations <- many (try parseGlobalVar <|> try parseGlobalFn <|> parseFnMain)
    eof
    return global_declarations

parseGlobalVar :: Parsec [Token] st VarDecl
parseGlobalVar = do 


-- invocação do parser para o símbolo de partida 

parser :: [Token] -> Either ParseError [Token]
parser tokens = runParser program () "Error message" tokens

