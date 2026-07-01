module Pressure.Typechecker.StructTest (structTypeTests) where

import Control.Monad (void)
import Pressure.Language.Lexer (runAlex)
import Pressure.Language.Parser (parseProgram)
import Pressure.TestUtil (assertLeft, assertOk)
import Pressure.Typechecker (checkProgram)
import Pressure.Typechecker.Error (Error)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

structTypeTests :: TestTree
structTypeTests =
  testGroup
    "structs"
    [ testCase "checks struct types and member access" testStructTypes
    ]

checkSource :: String -> Either Error ()
checkSource source = case runAlex fullSource parseProgram of
  Left err -> error $ "parse failed: " ++ err
  Right ast -> void (checkProgram ast)
  where
    fullSource = if "main" `elem` words (map (\c -> if c `elem` "():;,{}" then ' ' else c) source)
                   then source
                   else source ++ " main :: fn() {};"

testStructTypes :: IO ()
testStructTypes = do
  -- 1. Correct struct parameter and member access type checks
  assertOk "struct parameter and member access type checks" $
    checkSource "f :: fn(p: struct { x: i32, y: bool }) -> i32 { p.x };"

  -- 2. Duplicate field detection in struct definition
  assertLeft "duplicate fields in struct definition rejected" $
    checkSource "f :: fn(p: struct { x: i32, x: bool }) -> i32 { 0 };"

  -- 3. Accessing non-existent field rejected
  assertLeft "accessing non-existent field rejected" $
    checkSource "f :: fn(p: struct { x: i32, y: bool }) -> i32 { p.z };"

  -- 4. Accessing field on non-struct type rejected
  assertLeft "accessing field on non-struct type rejected" $
    checkSource "f :: fn(n: i32) -> i32 { n.x };"

  -- 5. Anonymous struct initialization type checks
  assertOk "anonymous struct initialization type checks" $
    checkSource "x := .{ x = 1, y = true };"

  -- 6. Named struct declaration and initialization type checks
  assertOk "named struct declaration and initialization type checks" $
    checkSource "MyStruct :: struct { x: i32, y: bool }; s := MyStruct { x = 1, y = true };"

  -- 7. Duplicate field in anonymous struct initialization rejected
  assertLeft "duplicate field in anonymous struct initialization rejected" $
    checkSource "x := .{ x = 1, x = 2 };"

  -- 8. Duplicate field in named struct initialization rejected
  assertLeft "duplicate field in named struct initialization rejected" $
    checkSource "MyStruct :: struct { x: i32 }; s := MyStruct { x = 1, x = 2 };"

  -- 9. Type mismatch in named struct initialization rejected
  assertLeft "type mismatch in named struct initialization rejected" $
    checkSource "MyStruct :: struct { x: i32 }; s := MyStruct { x = true };"

  -- 10. Unknown field in named struct initialization rejected
  assertLeft "unknown field in named struct initialization rejected" $
    checkSource "MyStruct :: struct { x: i32 }; s := MyStruct { y = 1 };"

  -- 11. Initialization of undefined struct type rejected
  assertLeft "initialization of undefined struct type rejected" $
    checkSource "s := UnknownStruct { x = 1 };"

  -- 12. Structural compatibility anonymous to named
  assertOk "assigning structurally compatible anonymous struct to named struct variable" $
    checkSource "MyStruct :: struct { x: i32, y: bool }; s : MyStruct = .{ x = 1, y = true };"

  -- 13. Structural incompatibility anonymous to named rejected
  assertLeft "assigning structurally incompatible anonymous struct to named struct variable" $
    checkSource "MyStruct :: struct { x: i32, y: bool }; s : MyStruct = .{ x = 1, z = true };"

  -- 14. Structural compatibility of anonymous structs
  assertOk "structural compatibility of anonymous structs" $
    checkSource "f :: fn(p: struct { x: i32, y: bool }) -> struct { x: i32, y: bool } { p };"

  -- 14b. Structural compatibility of anonymous structs with different order
  assertOk "structural compatibility of anonymous structs with different order" $
    checkSource "f :: fn(p: struct { y: bool, x: i32 }) -> struct { x: i32, y: bool } { p };"

  -- 15. Structural incompatibility of anonymous structs rejected
  assertLeft "structural incompatibility of anonymous structs rejected" $
    checkSource "f :: fn(p: struct { x: i32, y: bool }) -> struct { x: i32, z: bool } { p };"

  -- 16. Member access type mismatch rejected
  assertLeft "member access type mismatch rejected" $
    checkSource "f :: fn(p: struct { x: i32, y: bool }) -> i32 { p.y };"

  -- 17. Declaring and typechecking struct members/constants
  assertOk "declaring struct constant member and accessing it via dot" $
    checkSource "MyStruct :: struct { x: i32, y: bool, CONSTANTE :: 42; }; c : i32 = MyStruct.CONSTANTE;"

  -- 18. Declaring and typechecking struct methods with self reference
  assertOk "declaring struct method and accessing it via dot" $
    checkSource "MyStruct :: struct { x: i32, y: bool, getX :: fn(self: MyStruct) -> i32 { self.x }; };"

  -- 19. Function referencing a struct type defined in same scope (after the function)
  assertOk "function referencing struct type defined later in the same scope" $
    checkSource "f :: fn(s: MyStruct) -> i32 { s.x }; MyStruct :: struct { x: i32 };"

  -- 20. Assigning to field of constant struct is permitted
  assertOk "assigning to a field of a constant struct is permitted" $
    checkSource "MyStruct :: struct { x: i32 }; main :: fn() { s : MyStruct : .{ x = 1 }; s.x = 2; };"

  -- 21. Reassigning a constant struct variable is rejected
  assertLeft "reassigning a constant struct variable is rejected" $
    checkSource "MyStruct :: struct { x: i32 }; main :: fn() { s : MyStruct : .{ x = 1 }; s = MyStruct { x = 2 }; };"

  -- 22. Assigning to a struct constant member is rejected
  assertLeft "assigning to a struct constant member is rejected" $
    checkSource "MyStruct :: struct { x: i32, CONSTANTE :: 42; }; main :: fn() { s := MyStruct { x = 1 }; s.CONSTANTE = 43; };"
