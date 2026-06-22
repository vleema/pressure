module AstTest (astTests) where

import Ast.ArithTest (arithTests)
import Ast.AssignTest (assignTests)
import Ast.ControlTest (controlTests)
import Ast.ErrorTest (errorTests)
import Ast.FunctionTest (functionTests)
import Ast.LiteralTest (literalTests)
import Test.Tasty (TestTree, testGroup)

astTests :: TestTree
astTests =
  testGroup
    "ast"
    [ literalTests,
      arithTests,
      assignTests,
      errorTests,
      controlTests,
      functionTests
    ]
