module Pressure.Interpreter.Value where

import Data.List (intercalate)
import Data.Map.Strict (Map)
import Pressure.Language.Ast (TypedBlock, TypedParam)
import Pressure.Language.Types

type ValueEnv = [Map String Value]

data Value
  = VInt Sign IntSize Integer
  | VFloat FloatSize Double
  | VBool Bool
  | VString String
  | VUnit
  | VType Type
  | VEmpty
  | VFunction [TypedParam] Type TypedBlock ValueEnv
  | VArray [Value]
  | VBuiltin String
  | VStruct [(String, Value)]
  deriving (Eq)

instance Show Value where
  show = \case
    VInt _ _ i -> show i
    VFloat _ f -> show f
    VBool True -> "true"
    VBool False -> "false"
    VString s -> show s
    VUnit -> "()"
    VType t -> prettyType t
    VFunction {} -> "<function>"
    VArray list -> show list
    VBuiltin n -> "<builtin " ++ n ++ ">"
    VStruct fields -> "struct { " ++ intercalate ", " (map (\(n, v) -> n ++ " = " ++ show v) fields) ++ " }"
    VEmpty -> undefined
