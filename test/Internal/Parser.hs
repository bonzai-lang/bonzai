{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Internal.Parser where

import Language.Bonzai.Syntax.HLIR qualified as HLIR
import Language.Bonzai.Frontend.Parser.Internal.Literal qualified as Lit
import Language.Bonzai.Frontend.Parser.Expression qualified as P
import Control.Monad.Parser qualified as P
import Test.Hspec
import Internal.Utils

import Prelude hiding (bool, on)
import qualified Language.Bonzai.Frontend.Typechecking.Monad as Tc

testLiteral :: Spec
testLiteral = do
  it "parses an integer literal" $ do
    let input = "42"
    let expected = HLIR.MkLitInt 42
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "0"
    let expected = HLIR.MkLitInt 0
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "-42"
    let expected = HLIR.MkLitInt (-42)
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "-0"
    let expected = HLIR.MkLitInt 0
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "42_000"
    let expected = HLIR.MkLitInt 42000
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "42_000_000"
    let expected = HLIR.MkLitInt 42000000
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "420_00"
    let expected = HLIR.MkLitInt 42000
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

  it "parses a floating literal" $ do
    let input = "42.0"
    let expected = HLIR.MkLitFloat 42.0
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "0.0"
    let expected = HLIR.MkLitFloat 0.0
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "-42.0"
    let expected = HLIR.MkLitFloat (-42.0)
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "-0.0"
    let expected = HLIR.MkLitFloat 0.0
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "42_000.0"
    let expected = HLIR.MkLitFloat 42000.0
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "42_000_000.0"
    let expected = HLIR.MkLitFloat 42000000.0
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "420_00.0"
    let expected = HLIR.MkLitFloat 42000.0
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "42.0_0"
    let expected = HLIR.MkLitFloat 42.00
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "42.0_0_1"
    let expected = HLIR.MkLitFloat 42.001
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

  it "parses a character literal" $ do
    let input = "'a'"
    let expected = HLIR.MkLitChar 'a'
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "'\\n'"
    let expected = HLIR.MkLitChar '\n'
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "'\\t'"
    let expected = HLIR.MkLitChar '\t'
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "'\\r'"
    let expected = HLIR.MkLitChar '\r'
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "'\\''"
    let expected = HLIR.MkLitChar '\''
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "'\\\"'"
    let expected = HLIR.MkLitChar '\"'
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "'\\\\'"
    let expected = HLIR.MkLitChar '\\'
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "'c"
    res <- P.parseTestContent Lit.parseLiteral input
    shouldBeError res

    let input = "'\\"
    res <- P.parseTestContent Lit.parseLiteral input
    shouldBeError res

  it "parses a string literal" $ do
    let input = "\"hello\""
    let expected = HLIR.MkExprString "hello"
    res <- P.parseTestContent P.parseInterpolatedString input
    res `shouldBeRight` expected

    let input = "\"hello world\""
    let expected = HLIR.MkExprString "hello world"
    res <- P.parseTestContent P.parseInterpolatedString input
    res `shouldBeRight` expected

    let input = "\"hello\\nworld\""
    let expected = HLIR.MkExprString "hello\nworld"
    res <- P.parseTestContent P.parseInterpolatedString input
    res `shouldBeRight` expected

    let input = "\"hello\\tworld\""
    let expected = HLIR.MkExprString "hello\tworld"
    res <- P.parseTestContent P.parseInterpolatedString input
    res `shouldBeRight` expected

    let input = "\"hello\\rworld\""
    let expected = HLIR.MkExprString "hello\rworld"
    res <- P.parseTestContent P.parseInterpolatedString input
    res `shouldBeRight` expected

    let input = "\"hello\\\"world\""
    let expected = HLIR.MkExprString "hello\"world"
    res <- P.parseTestContent P.parseInterpolatedString input
    res `shouldBeRight` expected

    let input = "\"hello\\'world\""
    let expected = HLIR.MkExprString "hello'world"
    res <- P.parseTestContent P.parseInterpolatedString input
    res `shouldBeRight` expected

    let input = "\"hello\\\\world\""
    let expected = HLIR.MkExprString "hello\\world"
    res <- P.parseTestContent P.parseInterpolatedString input
    res `shouldBeRight` expected

    let input = "\"hello"
    res <- P.parseTestContent P.parseInterpolatedString input
    shouldBeError res

    let input = "\"hello\\"
    res <- P.parseTestContent P.parseInterpolatedString input
    shouldBeError res

    let input = "\"hello\\a"
    res <- P.parseTestContent P.parseInterpolatedString input
    shouldBeError res

testExpression :: Spec
testExpression = do
  describe "parseLiteral" testLiteral

  it "parses a variable" $ do
    let input = "x"
    let expected = HLIR.MkExprVariable $ HLIR.MkAnnotation "x" Nothing
    res <- P.parseTestContent P.parseVariable input
    res `shouldBeRight'` expected

    let input = "x_"
    let expected = HLIR.MkExprVariable $ HLIR.MkAnnotation "x_" Nothing
    res <- P.parseTestContent P.parseVariable input
    res `shouldBeRight'` expected

    let input = "x_0"
    let expected = HLIR.MkExprVariable $ HLIR.MkAnnotation "x_0" Nothing
    res <- P.parseTestContent P.parseVariable input
    res `shouldBeRight'` expected

    let input = "x0"
    let expected = HLIR.MkExprVariable $ HLIR.MkAnnotation "x0" Nothing
    res <- P.parseTestContent P.parseVariable input
    res `shouldBeRight'` expected

    let input = "x0_"
    let expected = HLIR.MkExprVariable $ HLIR.MkAnnotation "x0_" Nothing
    res <- P.parseTestContent P.parseVariable input
    res `shouldBeRight'` expected

    let input = "x0_0"
    let expected = HLIR.MkExprVariable $ HLIR.MkAnnotation "x0_0" Nothing
    res <- P.parseTestContent P.parseVariable input
    res `shouldBeRight'` expected

    let input = "x0_0_"
    let expected = HLIR.MkExprVariable $ HLIR.MkAnnotation "x0_0_" Nothing
    res <- P.parseTestContent P.parseVariable input
    res `shouldBeRight'` expected

    let input = "x0_0_0"
    let expected = HLIR.MkExprVariable $ HLIR.MkAnnotation "x0_0_0" Nothing
    res <- P.parseTestContent P.parseVariable input
    res `shouldBeRight'` expected

    let input = "x0_0_0_"
    let expected = HLIR.MkExprVariable $ HLIR.MkAnnotation "x0_0_0_" Nothing
    res <- P.parseTestContent P.parseVariable input
    res `shouldBeRight'` expected

    let input = "x0_0_0_0"
    let expected = HLIR.MkExprVariable $ HLIR.MkAnnotation "x0_0_0_0" Nothing
    res <- P.parseTestContent P.parseVariable input
    res `shouldBeRight'` expected

    let input = "0x"
    res <- P.parseTestContent P.parseVariable input
    shouldBeError res

    let input = "0x0"
    res <- P.parseTestContent P.parseVariable input
    shouldBeError res

  it "parses a list" $ do
    let input = "[]"
    let expected = HLIR.MkExprList []
    res <- P.parseTestContent P.parseList input
    res `shouldBeRight'` expected

    let input = "[1]"
    let expected = HLIR.MkExprList [HLIR.MkExprLiteral $ HLIR.MkLitInt 1]
    res <- P.parseTestContent P.parseList input
    res `shouldBeRight'` expected

    let input = "[1, 2]"
    let expected = HLIR.MkExprList [HLIR.MkExprLiteral $ HLIR.MkLitInt 1, HLIR.MkExprLiteral $ HLIR.MkLitInt 2]
    res <- P.parseTestContent P.parseList input
    res `shouldBeRight'` expected

    let input = "[1, 2, 3]"
    let expected = HLIR.MkExprList [HLIR.MkExprLiteral $ HLIR.MkLitInt 1, HLIR.MkExprLiteral $ HLIR.MkLitInt 2, HLIR.MkExprLiteral $ HLIR.MkLitInt 3]
    res <- P.parseTestContent P.parseList input
    res `shouldBeRight'` expected

    let input = "[1, 2, 3, 4]"
    let expected = HLIR.MkExprList [HLIR.MkExprLiteral $ HLIR.MkLitInt 1, HLIR.MkExprLiteral $ HLIR.MkLitInt 2, HLIR.MkExprLiteral $ HLIR.MkLitInt 3, HLIR.MkExprLiteral $ HLIR.MkLitInt 4]
    res <- P.parseTestContent P.parseList input
    res `shouldBeRight'` expected

    let input = "[1, 2, 3, 4, 5]"
    let expected = HLIR.MkExprList [HLIR.MkExprLiteral $ HLIR.MkLitInt 1, HLIR.MkExprLiteral $ HLIR.MkLitInt 2, HLIR.MkExprLiteral $ HLIR.MkLitInt 3, HLIR.MkExprLiteral $ HLIR.MkLitInt 4, HLIR.MkExprLiteral $ HLIR.MkLitInt 5]
    res <- P.parseTestContent P.parseList input
    res `shouldBeRight'` expected

  it "parses a ternary expression" $ do
    let input = "if true then 1 else 0"
    let expected = ifThenElse (bool True) (int 1) (int 0)
    res <- P.parseTestContent P.parseTernary input
    res `shouldBeRight'` expected

    let input = "if false then 1 else 0"
    let expected = ifThenElse (bool False) (int 1) (int 0)
    res <- P.parseTestContent P.parseTernary input
    res `shouldBeRight'` expected

    let input = "if true then 1 else if false then 0 else 2"
    let expected = ifThenElse (bool True) (int 1) (ifThenElse (bool False) (int 0) (int 2))
    res <- P.parseTestContent P.parseTernary input
    res `shouldBeRight'` expected

    let input = "if true then if false then 0 else 1 else 2"
    let expected = ifThenElse (bool True) (ifThenElse (bool False) (int 0) (int 1)) (int 2)
    res <- P.parseTestContent P.parseTernary input
    res `shouldBeRight'` expected

    let input = "if true then 1 else"
    res <- P.parseTestContent P.parseTernary input
    shouldBeError res

    let input = "if true then"
    res <- P.parseTestContent P.parseTernary input
    shouldBeError res

    let input = "if"
    res <- P.parseTestContent P.parseTernary input
    shouldBeError res

  it "parses let expression" $ do
    let input = "let x = 1"
    let expected = HLIR.MkExprLet mempty (Left (HLIR.MkAnnotation "x" Nothing)) (HLIR.MkExprLiteral $ HLIR.MkLitInt 1) (HLIR.MkExprVariable (HLIR.MkAnnotation "unit" Nothing))
    res <- P.parseTestContent P.parseLet input
    res `shouldBeRight'` expected

    let input = "let x = "
    res <- P.parseTestContent P.parseLet input
    shouldBeError res

    let input = "let x"
    res <- P.parseTestContent P.parseLet input
    shouldBeError res

    let input = "let"
    res <- P.parseTestContent P.parseLet input
    shouldBeError res

  it "parses mutable expression" $ do
    let input = "mut x = 1"
    let expected = HLIR.MkExprLet mempty (Left (HLIR.MkAnnotation "x" Nothing)) (HLIR.MkExprMut (HLIR.MkExprLiteral $ HLIR.MkLitInt 1)) (HLIR.MkExprVariable (HLIR.MkAnnotation "unit" Nothing))
    res <- P.parseTestContent P.parseMut input
    res `shouldBeRight'` expected

    let input = "mut x = "
    res <- P.parseTestContent P.parseMut input
    shouldBeError res

    let input = "mut x"
    res <- P.parseTestContent P.parseMut input
    shouldBeError res

    let input = "mut"
    res <- P.parseTestContent P.parseMut input
    shouldBeError res

  it "parses a while expression" $ do
    let input = "while true { 1 }"
    let expected = HLIR.MkExprWhile (bool True) (HLIR.MkExprBlock [int 1])
    res <- P.parseTestContent P.parseWhile input
    res `shouldBeRight'` expected

    let input = "while true { 1"
    res <- P.parseTestContent P.parseWhile input
    shouldBeError res

    let input = "while true {"
    res <- P.parseTestContent P.parseWhile input
    shouldBeError res

    let input = "while true"
    res <- P.parseTestContent P.parseWhile input
    shouldBeError res

    let input = "while"
    res <- P.parseTestContent P.parseWhile input
    shouldBeError res

    let input = "while true { 1 2 3 }"
    let expected = HLIR.MkExprWhile (bool True) (HLIR.MkExprBlock [int 1, int 2, int 3])
    res <- P.parseTestContent P.parseWhile input
    res `shouldBeRight'` expected

  it "parses a block of expressions" $ do
    let input = "{}"
    let expected = HLIR.MkExprBlock []
    res <- P.parseTestContent P.parseBlock input
    res `shouldBeRight'` expected

    let input = "{ 1 }"
    let expected = HLIR.MkExprBlock [int 1]
    res <- P.parseTestContent P.parseBlock input
    res `shouldBeRight'` expected

    let input = "{ 1 2 }"
    let expected = HLIR.MkExprBlock [int 1, int 2]
    res <- P.parseTestContent P.parseBlock input
    res `shouldBeRight'` expected

    let input = "{ 1 2 3 }"
    let expected = HLIR.MkExprBlock [int 1, int 2, int 3]
    res <- P.parseTestContent P.parseBlock input
    res `shouldBeRight'` expected

    let input = "{ 1 2 3 4 }"
    let expected = HLIR.MkExprBlock [int 1, int 2, int 3, int 4]
    res <- P.parseTestContent P.parseBlock input
    res `shouldBeRight'` expected

    let input = "{ "
    res <- P.parseTestContent P.parseBlock input
    shouldBeError res

    let input = "{ 1"
    res <- P.parseTestContent P.parseBlock input
    shouldBeError res

  it "parses a function definition" $ do
    let input = "fn f() => 1"
    let expected = function "f" ["kwargs"] (int 1)
    res <- P.parseTestContent P.parseFunction input
    res `shouldBeRight'` expected

    let input = "fn f() =>"
    res <- P.parseTestContent P.parseFunction input
    shouldBeError res

    let input = "fn f() "
    res <- P.parseTestContent P.parseFunction input
    shouldBeError res

    let input = "fn fun(x, y) => 1"
    let expected = function "fun" ["x", "y", "kwargs"] (int 1)
    res <- P.parseTestContent P.parseFunction input
    res `shouldBeRight'` expected

    let input = "fn fun(x, y) =>"
    res <- P.parseTestContent P.parseFunction input
    shouldBeError res

    let input = "fn xyz(x, 1) => 1"
    res <- P.parseTestContent P.parseFunction input
    let expected = function "xyz" ["x", "$symbol_0", "kwargs"] $ do
          HLIR.MkExprMatch 
            (HLIR.MkExprVariable $ HLIR.MkAnnotation "$symbol_0" Nothing)
            [ (HLIR.MkPatLiteral (HLIR.MkLitInt 1), int 1, Nothing) ]
    print ((removeLocation <$>) <$> res)
    res `shouldBeRight'` expected

    let input = "fn xyz(1, y) => 1"
    res <- P.parseTestContent P.parseFunction input
    let expected = function "xyz" ["$symbol_0", "y", "kwargs"] $ do
          HLIR.MkExprMatch 
            (HLIR.MkExprVariable $ HLIR.MkAnnotation "$symbol_0" Nothing)
            [ (HLIR.MkPatLiteral (HLIR.MkLitInt 1), int 1, Nothing) ]
    res `shouldBeRight'` expected

    let input = "fn xyz(x, y) =>"
    res <- P.parseTestContent P.parseFunction input
    shouldBeError res

  it "parses a lambda expression" $ do
    let input = "fn() => 1"
    let expected = lambda ["kwargs"] (int 1)
    res <- P.parseTestContent P.parseLambda input
    res `shouldBeRight'` expected

    let input = "fn() =>"
    res <- P.parseTestContent P.parseLambda input
    shouldBeError res

    let input = "fn() "
    res <- P.parseTestContent P.parseLambda input
    shouldBeError res

    let input = "fn(x, y) => 1"
    let expected = lambda ["x", "y", "kwargs"] (int 1)
    res <- P.parseTestContent P.parseLambda input
    res `shouldBeRight'` expected

    let input = "fn(x, y) =>"
    res <- P.parseTestContent P.parseLambda input
    shouldBeError res

    let input = "fn(x, 1) => 1"
    res <- P.parseTestContent P.parseLambda input
    let expected = lambda ["x", "$symbol_0", "kwargs"] $ do
          HLIR.MkExprMatch 
            (HLIR.MkExprVariable $ HLIR.MkAnnotation "$symbol_0" Nothing)
            [ (HLIR.MkPatLiteral (HLIR.MkLitInt 1), int 1, Nothing) ]
    res `shouldBeRight'` expected

    let input = "fn(1, y) => 1"
    res <- P.parseTestContent P.parseLambda input
    let expected = lambda ["$symbol_0", "y", "kwargs"] $ do
          HLIR.MkExprMatch 
            (HLIR.MkExprVariable $ HLIR.MkAnnotation "$symbol_0" Nothing)
            [ (HLIR.MkPatLiteral (HLIR.MkLitInt 1), int 1, Nothing) ]
    res `shouldBeRight'` expected

  it "parses an update expression" $ do
    let input = "x = 1"
    let expected = HLIR.MkExprUpdate (HLIR.MkUpdtVariable (HLIR.MkAnnotation "x" Nothing)) (int 1)
    res <- P.parseTestContent P.parseUpdate input
    res `shouldBeRight'` expected

    let input = "x ="
    res <- P.parseTestContent P.parseUpdate input
    shouldBeError res

    let input = "x"
    res <- P.parseTestContent P.parseUpdate input
    shouldBeError res

    let input = "="
    res <- P.parseTestContent P.parseUpdate input
    shouldBeError res

  it "parses an expression" $ do
    let input = "1"
    let expected = int 1
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "true"
    let expected = bool True
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "false"
    let expected = bool False
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "42"
    let expected = int 42
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "42.0"
    let expected = HLIR.MkExprLiteral $ HLIR.MkLitFloat 42.0
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "'a'"
    let expected = HLIR.MkExprLiteral $ HLIR.MkLitChar 'a'
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "\"hello\""
    let expected = HLIR.MkExprLiteral $ HLIR.MkLitString "hello"
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "x"
    let expected = HLIR.MkExprVariable $ HLIR.MkAnnotation "x" Nothing
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "[]"
    let expected = HLIR.MkExprList []
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "if true then 1 else 0"
    let expected = ifThenElse (bool True) (int 1) (int 0)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "let x = 1"
    let expected = HLIR.MkExprLet mempty (Left (HLIR.MkAnnotation "x" Nothing)) (int 1) (HLIR.MkExprVariable (HLIR.MkAnnotation "unit" Nothing))
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "mut x = 1"
    let expected = HLIR.MkExprLet mempty (Left (HLIR.MkAnnotation "x" Nothing)) (HLIR.MkExprMut (int 1)) (HLIR.MkExprVariable (HLIR.MkAnnotation "unit" Nothing))
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "{}"
    let expected = HLIR.MkExprRecordEmpty
    res <- P.parseTestContent P.parseExpression input
    
    case res of
      Left err -> expectationFailure $ "Expected Right, but got Left: " <> show err
      Right (_, actual) -> removeLocation actual `shouldBe` expected

    let input = "fn f() => 1"
    res <- P.parseTestContent P.parseExpression input
    shouldBeError res

    let input = "fn f() =>"
    res <- P.parseTestContent P.parseExpression input
    shouldBeError res

    let input = "fn f() "
    res <- P.parseTestContent P.parseExpression input
    shouldBeError res

    let input = "fn fun(x, y) => 1"
    res <- P.parseTestContent P.parseExpression input
    shouldBeError res

    let input = "fn fun(x, y) =>"
    res <- P.parseTestContent P.parseExpression input
    shouldBeError res

    let input = "fn (x, y) => 1"
    let expected = lambda ["x", "y", "kwargs"] (int 1)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "fn (x, y) =>"
    res <- P.parseTestContent P.parseExpression input
    shouldBeError res

    let input = "f(x, y, z)"
    let expected = HLIR.MkExprApplication (var "f") [var "x", var "y", var "z", HLIR.MkExprRecordEmpty]
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "x.f"
    let expected = HLIR.MkExprApplication (var "f") [var "x", HLIR.MkExprRecordEmpty]
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "x.f()"
    let expected = HLIR.MkExprApplication (var "f") [var "x", HLIR.MkExprRecordEmpty]
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "x.f(x)"
    let expected = HLIR.MkExprApplication (var "f") [var "x", var "x", HLIR.MkExprRecordEmpty]
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "x.y.z"
    let expected = HLIR.MkExprApplication (var "z") [HLIR.MkExprApplication (var "y") [var "x", HLIR.MkExprRecordEmpty], HLIR.MkExprRecordEmpty]
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "x.y.z()"
    let expected = HLIR.MkExprApplication (var "z") [HLIR.MkExprApplication (var "y") [var "x", HLIR.MkExprRecordEmpty], HLIR.MkExprRecordEmpty] 
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "x.y.z(x)"
    let expected = HLIR.MkExprApplication (var "z") [HLIR.MkExprApplication (var "y") [var "x", HLIR.MkExprRecordEmpty], var "x", HLIR.MkExprRecordEmpty]
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "x.y.z(x, y)"
    let expected = HLIR.MkExprApplication (var "z") [HLIR.MkExprApplication (var "y") [var "x", HLIR.MkExprRecordEmpty], var "x", var "y", HLIR.MkExprRecordEmpty]
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "x(y)(z)"
    let expected = HLIR.MkExprApplication (HLIR.MkExprApplication (var "x") [var "y", HLIR.MkExprRecordEmpty]) [var "z", HLIR.MkExprRecordEmpty]
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "1 * 2"
    let expected = HLIR.MkExprBinary "*" (int 1) (int 2)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "1 + 2"
    let expected = HLIR.MkExprBinary "+" (int 1) (int 2)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "1 - 2"
    let expected = HLIR.MkExprBinary "-" (int 1) (int 2)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "1 / 2"
    let expected = HLIR.MkExprBinary "/" (int 1) (int 2)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "1 * 2 + 3"
    let expected = HLIR.MkExprBinary "+" (HLIR.MkExprBinary "*" (int 1) (int 2)) (int 3)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "1 + 2 * 3"
    let expected = HLIR.MkExprBinary "+" (int 1) (HLIR.MkExprBinary "*" (int 2) (int 3))
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "1 == 2"
    let expected = HLIR.MkExprBinary "==" (int 1) (int 2)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "1 != 2"
    let expected = HLIR.MkExprBinary "!=" (int 1) (int 2)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "1 < 2 + 2"
    let expected = HLIR.MkExprBinary "<" (int 1) (HLIR.MkExprBinary "+" (int 2) (int 2))
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "1 <= 2 + 2"
    let expected = HLIR.MkExprBinary "<=" (int 1) (HLIR.MkExprBinary "+" (int 2) (int 2))
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "1 > 2 && 2 < 3"
    let expected = HLIR.MkExprBinary "&&" (HLIR.MkExprBinary ">" (int 1) (int 2)) (HLIR.MkExprBinary "<" (int 2) (int 3))
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "1 || 2 && 2"
    let expected = HLIR.MkExprBinary "&&" (HLIR.MkExprBinary "||" (int 1) (int 2)) (int 2)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    -- Custom operators right associative

    let input = "x %= 1"
    let expected = HLIR.MkExprBinary "%=" (var "x") (int 1)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

    let input = "x % 2 % 3"
    let expected = HLIR.MkExprBinary "%" (HLIR.MkExprBinary "%" (var "x") (int 2)) (int 3)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight'` expected

programParser :: Spec 
programParser = do
  it "parses a require expression" $ do
    let input = "require 1"
    res <- P.parseTestContent P.parseRequire input
    shouldBeError res

    let input = "require"
    res <- P.parseTestContent P.parseRequire input
    shouldBeError res

    let input = "require \"hello\""
    let expected = HLIR.MkExprRequire "hello" mempty
    res <- P.parseTestContent P.parseRequire input
    res `shouldBeRight'` expected

  it "parses extern declaration" $ do
    let input = "extern fn f(x: int): int"
    recTy <- Tc.fresh
    let funTy = [HLIR.MkTyInt, recTy] HLIR.:->: HLIR.MkTyInt
    let expected = HLIR.MkExprNative (HLIR.MkAnnotation "f" []) funTy
    res <- P.parseTestContent P.parseExtern input
    res `shouldBeRight'` expected

    let input = "extern fn f(x: int)"
    res <- P.parseTestContent P.parseExtern input
    shouldBeError res

    let input = "extern fn f(x: int, y: int): string"
    recTy <- Tc.fresh
    let funTy = [HLIR.MkTyInt, HLIR.MkTyInt, recTy] HLIR.:->: HLIR.MkTyString
    let expected = HLIR.MkExprNative (HLIR.MkAnnotation "f" []) funTy
    res <- P.parseTestContent P.parseExtern input
    res `shouldBeRight'` expected

    let input = "extern fn f<A>(x: int, y: A): A"
    recTy <- Tc.fresh
    let funTy = [HLIR.MkTyInt, HLIR.MkTyId "A", recTy] HLIR.:->: HLIR.MkTyId "A"
    let expected = HLIR.MkExprNative (HLIR.MkAnnotation "f" ["A"]) funTy
    res <- P.parseTestContent P.parseExtern input
    res `shouldBeRight'` expected

    let input = "extern fn f<A, B>(x: A, y: B): A"
    recTy <- Tc.fresh
    let funTy = [HLIR.MkTyId "A", HLIR.MkTyId "B", recTy] HLIR.:->: HLIR.MkTyId "A"
    let expected = HLIR.MkExprNative (HLIR.MkAnnotation "f" ["A", "B"]) funTy
    res <- P.parseTestContent P.parseExtern input
    res `shouldBeRight'` expected

  it "parses a program" $ do
    let input = "fn main() => 1"
    let expected = [function "main" ["kwargs"] (int 1)]
    res <- P.parseTestContent P.parseProgram input
    res `shouldBeRight` expected

    let input = "fn main() => 1 fn test() => 2"
    let expected = [function "main" ["kwargs"] (int 1), function "test" ["kwargs"] (int 2)]
    res <- P.parseTestContent P.parseProgram input
    res `shouldBeRight` expected

    let input = "fn main() => 1 fn test() => 2 fn test2() => 3"
    let expected = [function "main" ["kwargs"] (int 1), function "test" ["kwargs"] (int 2), function "test2" ["kwargs"] (int 3)]
    res <- P.parseTestContent P.parseProgram input
    res `shouldBeRight` expected

    let input = "fn main() => 1 fn test() => 2 fn test2() => 3 fn test3() => 4"
    let expected = [function "main" ["kwargs"] (int 1), function "test" ["kwargs"] (int 2), function "test2" ["kwargs"] (int 3), function "test3" ["kwargs"] (int 4)]
    res <- P.parseTestContent P.parseProgram input
    res `shouldBeRight` expected

    let input = "fn main() => 1 fn test() => 2 fn test2() => 3 fn test3() => 4 fn test4() => 5"
    let expected = [function "main" ["kwargs"] (int 1), function "test" ["kwargs"] (int 2), function "test2" ["kwargs"] (int 3), function "test3" ["kwargs"] (int 4), function "test4" ["kwargs"] (int 5)]
    res <- P.parseTestContent P.parseProgram input
    res `shouldBeRight` expected

    let input = "extern fn f(x: int): int"
    let funTy = [HLIR.MkTyInt] HLIR.:->: HLIR.MkTyInt
    let expected = [HLIR.MkExprNative (HLIR.MkAnnotation "f" []) funTy]
    res <- P.parseTestContent P.parseProgram input
    res `shouldBeRight` expected

    let input = "extern fn f(x: int): int extern fn f2(x: int): int"
    let funTy = [HLIR.MkTyInt] HLIR.:->: HLIR.MkTyInt
    let expected = [HLIR.MkExprNative (HLIR.MkAnnotation "f" []) funTy, HLIR.MkExprNative (HLIR.MkAnnotation "f2" []) funTy]
    res <- P.parseTestContent P.parseProgram input
    res `shouldBeRight` expected

testParser :: Spec
testParser = do
  describe "expression parser" testExpression
  describe "program parser" programParser