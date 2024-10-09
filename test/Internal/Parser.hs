{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Internal.Parser where

import Language.Bonzai.Syntax.HLIR qualified as HLIR
import Language.Bonzai.Frontend.Parser.Internal.Literal qualified as Lit
import Language.Bonzai.Frontend.Parser.Expression qualified as P
import Control.Monad.Parser qualified as P
import Test.Hspec
import Internal.Utils

import Prelude hiding (bool, on)

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
    let expected = HLIR.MkLitString "hello"
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "\"hello world\""
    let expected = HLIR.MkLitString "hello world"
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "\"hello\\nworld\""
    let expected = HLIR.MkLitString "hello\nworld"
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "\"hello\\tworld\""
    let expected = HLIR.MkLitString "hello\tworld"
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "\"hello\\rworld\""
    let expected = HLIR.MkLitString "hello\rworld"
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "\"hello\\\"world\""
    let expected = HLIR.MkLitString "hello\"world"
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "\"hello\\'world\""
    let expected = HLIR.MkLitString "hello'world"
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "\"hello\\\\world\""
    let expected = HLIR.MkLitString "hello\\world"
    res <- P.parseTestContent Lit.parseLiteral input
    res `shouldBeRight` expected

    let input = "\"hello"
    res <- P.parseTestContent Lit.parseLiteral input
    shouldBeError res

    let input = "\"hello\\"
    res <- P.parseTestContent Lit.parseLiteral input
    shouldBeError res

    let input = "\"hello\\a"
    res <- P.parseTestContent Lit.parseLiteral input
    shouldBeError res

testExpression :: Spec
testExpression = do
  describe "parseLiteral" testLiteral

  it "parses a variable" $ do
    let input = "x"
    let expected = HLIR.MkExprVariable $ HLIR.MkAnnotation "x" Nothing
    res <- P.parseTestContent P.parseVariable input
    res `shouldBeRight` expected

    let input = "x_"
    let expected = HLIR.MkExprVariable $ HLIR.MkAnnotation "x_" Nothing
    res <- P.parseTestContent P.parseVariable input
    res `shouldBeRight` expected

    let input = "x_0"
    let expected = HLIR.MkExprVariable $ HLIR.MkAnnotation "x_0" Nothing
    res <- P.parseTestContent P.parseVariable input
    res `shouldBeRight` expected

    let input = "x0"
    let expected = HLIR.MkExprVariable $ HLIR.MkAnnotation "x0" Nothing
    res <- P.parseTestContent P.parseVariable input
    res `shouldBeRight` expected

    let input = "x0_"
    let expected = HLIR.MkExprVariable $ HLIR.MkAnnotation "x0_" Nothing
    res <- P.parseTestContent P.parseVariable input
    res `shouldBeRight` expected

    let input = "x0_0"
    let expected = HLIR.MkExprVariable $ HLIR.MkAnnotation "x0_0" Nothing
    res <- P.parseTestContent P.parseVariable input
    res `shouldBeRight` expected

    let input = "x0_0_"
    let expected = HLIR.MkExprVariable $ HLIR.MkAnnotation "x0_0_" Nothing
    res <- P.parseTestContent P.parseVariable input
    res `shouldBeRight` expected

    let input = "x0_0_0"
    let expected = HLIR.MkExprVariable $ HLIR.MkAnnotation "x0_0_0" Nothing
    res <- P.parseTestContent P.parseVariable input
    res `shouldBeRight` expected

    let input = "x0_0_0_"
    let expected = HLIR.MkExprVariable $ HLIR.MkAnnotation "x0_0_0_" Nothing
    res <- P.parseTestContent P.parseVariable input
    res `shouldBeRight` expected

    let input = "x0_0_0_0"
    let expected = HLIR.MkExprVariable $ HLIR.MkAnnotation "x0_0_0_0" Nothing
    res <- P.parseTestContent P.parseVariable input
    res `shouldBeRight` expected

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
    res `shouldBeRight` expected

    let input = "[1]"
    let expected = HLIR.MkExprList [HLIR.MkExprLiteral $ HLIR.MkLitInt 1]
    res <- P.parseTestContent P.parseList input
    res `shouldBeRight` expected

    let input = "[1, 2]"
    let expected = HLIR.MkExprList [HLIR.MkExprLiteral $ HLIR.MkLitInt 1, HLIR.MkExprLiteral $ HLIR.MkLitInt 2]
    res <- P.parseTestContent P.parseList input
    res `shouldBeRight` expected

    let input = "[1, 2, 3]"
    let expected = HLIR.MkExprList [HLIR.MkExprLiteral $ HLIR.MkLitInt 1, HLIR.MkExprLiteral $ HLIR.MkLitInt 2, HLIR.MkExprLiteral $ HLIR.MkLitInt 3]
    res <- P.parseTestContent P.parseList input
    res `shouldBeRight` expected

    let input = "[1, 2, 3, 4]"
    let expected = HLIR.MkExprList [HLIR.MkExprLiteral $ HLIR.MkLitInt 1, HLIR.MkExprLiteral $ HLIR.MkLitInt 2, HLIR.MkExprLiteral $ HLIR.MkLitInt 3, HLIR.MkExprLiteral $ HLIR.MkLitInt 4]
    res <- P.parseTestContent P.parseList input
    res `shouldBeRight` expected

    let input = "[1, 2, 3, 4, 5]"
    let expected = HLIR.MkExprList [HLIR.MkExprLiteral $ HLIR.MkLitInt 1, HLIR.MkExprLiteral $ HLIR.MkLitInt 2, HLIR.MkExprLiteral $ HLIR.MkLitInt 3, HLIR.MkExprLiteral $ HLIR.MkLitInt 4, HLIR.MkExprLiteral $ HLIR.MkLitInt 5]
    res <- P.parseTestContent P.parseList input
    res `shouldBeRight` expected

  it "parses a ternary expression" $ do
    let input = "if true then 1 else 0"
    let expected = ifThenElse (bool True) (int 1) (int 0)
    res <- P.parseTestContent P.parseTernary input
    res `shouldBeRight` expected

    let input = "if false then 1 else 0"
    let expected = ifThenElse (bool False) (int 1) (int 0)
    res <- P.parseTestContent P.parseTernary input
    res `shouldBeRight` expected

    let input = "if true then 1 else if false then 0 else 2"
    let expected = ifThenElse (bool True) (int 1) (ifThenElse (bool False) (int 0) (int 2))
    res <- P.parseTestContent P.parseTernary input
    res `shouldBeRight` expected

    let input = "if true then if false then 0 else 1 else 2"
    let expected = ifThenElse (bool True) (ifThenElse (bool False) (int 0) (int 1)) (int 2)
    res <- P.parseTestContent P.parseTernary input
    res `shouldBeRight` expected

    let input = "if true then 1 else"
    res <- P.parseTestContent P.parseTernary input
    shouldBeError res

    let input = "if true then 1"
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
    let expected = HLIR.MkExprLet (HLIR.MkAnnotation "x" Nothing) (HLIR.MkExprLiteral $ HLIR.MkLitInt 1)
    res <- P.parseTestContent P.parseLet input
    res `shouldBeRight` expected

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
    let expected = HLIR.MkExprMut (HLIR.MkAnnotation "x" Nothing) (HLIR.MkExprLiteral $ HLIR.MkLitInt 1)
    res <- P.parseTestContent P.parseMut input
    res `shouldBeRight` expected

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
    res `shouldBeRight` expected

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
    res `shouldBeRight` expected

  it "parses a block of expressions" $ do
    let input = "{}"
    let expected = HLIR.MkExprBlock []
    res <- P.parseTestContent P.parseBlock input
    res `shouldBeRight` expected

    let input = "{ 1 }"
    let expected = HLIR.MkExprBlock [int 1]
    res <- P.parseTestContent P.parseBlock input
    res `shouldBeRight` expected

    let input = "{ 1 2 }"
    let expected = HLIR.MkExprBlock [int 1, int 2]
    res <- P.parseTestContent P.parseBlock input
    res `shouldBeRight` expected

    let input = "{ 1 2 3 }"
    let expected = HLIR.MkExprBlock [int 1, int 2, int 3]
    res <- P.parseTestContent P.parseBlock input
    res `shouldBeRight` expected

    let input = "{ 1 2 3 4 }"
    let expected = HLIR.MkExprBlock [int 1, int 2, int 3, int 4]
    res <- P.parseTestContent P.parseBlock input
    res `shouldBeRight` expected

    let input = "{ "
    res <- P.parseTestContent P.parseBlock input
    shouldBeError res

    let input = "{ 1"
    res <- P.parseTestContent P.parseBlock input
    shouldBeError res

  it "parses a function definition" $ do
    let input = "fn f() => 1"
    let expected = function "f" [] (int 1)
    res <- P.parseTestContent P.parseFunction input
    res `shouldBeRight` expected

    let input = "fn f() =>"
    res <- P.parseTestContent P.parseFunction input
    shouldBeError res

    let input = "fn f() "
    res <- P.parseTestContent P.parseFunction input
    shouldBeError res

    let input = "fn fun(x, y) => 1"
    let expected = function "fun" ["x", "y"] (int 1)
    res <- P.parseTestContent P.parseFunction input
    res `shouldBeRight` expected

    let input = "fn fun(x, y) =>"
    res <- P.parseTestContent P.parseFunction input
    shouldBeError res

    let input = "fn xyz(x, 1) => 1"
    res <- P.parseTestContent P.parseFunction input
    shouldBeError res

    let input = "fn xyz(1, y) => 1"
    res <- P.parseTestContent P.parseFunction input
    shouldBeError res

    let input = "fn xyz(x, y) =>"
    res <- P.parseTestContent P.parseFunction input
    shouldBeError res

  it "parses a lambda expression" $ do
    let input = "fn() => 1"
    let expected = lambda [] (int 1)
    res <- P.parseTestContent P.parseLambda input
    res `shouldBeRight` expected

    let input = "fn() =>"
    res <- P.parseTestContent P.parseLambda input
    shouldBeError res

    let input = "fn() "
    res <- P.parseTestContent P.parseLambda input
    shouldBeError res

    let input = "fn(x, y) => 1"
    let expected = lambda ["x", "y"] (int 1)
    res <- P.parseTestContent P.parseLambda input
    res `shouldBeRight` expected

    let input = "fn(x, y) =>"
    res <- P.parseTestContent P.parseLambda input
    shouldBeError res

    let input = "fn(x, 1) => 1"
    res <- P.parseTestContent P.parseLambda input
    shouldBeError res

    let input = "fn(1, y) => 1"
    res <- P.parseTestContent P.parseLambda input
    shouldBeError res

  it "parses an update expression" $ do
    let input = "x = 1"
    let expected = HLIR.MkExprUpdate (HLIR.MkUpdtVariable (HLIR.MkAnnotation "x" Nothing)) (int 1)
    res <- P.parseTestContent P.parseUpdate input
    res `shouldBeRight` expected

    let input = "x ="
    res <- P.parseTestContent P.parseUpdate input
    shouldBeError res

    let input = "x"
    res <- P.parseTestContent P.parseUpdate input
    shouldBeError res

    let input = "="
    res <- P.parseTestContent P.parseUpdate input
    shouldBeError res

  it "parses an actor expression" $ do
    let input = "actor test < x { }"
    let expected = actor "test" "x" []
    res <- P.parseTestContent P.parseActor input
    res `shouldBeRight` expected

    let input = "actor test < x { 1 }"
    res <- P.parseTestContent P.parseActor input
    shouldBeError res

    let input = "actor test < x { 1 2 }"
    res <- P.parseTestContent P.parseActor input
    shouldBeError res

    let input = "actor < x { }"
    res <- P.parseTestContent P.parseActor input
    shouldBeError res

    let input = "actor test { }"
    res <- P.parseTestContent P.parseActor input
    shouldBeError res

  it "parses an anonymous actor expression" $ do
    let input = "actor < x { }"
    let expected = anonActor "x" []
    res <- P.parseTestContent P.parseAnonActor input
    res `shouldBeRight` expected

    let input = "actor < x { 1 }"
    res <- P.parseTestContent P.parseAnonActor input
    shouldBeError res

    let input = "actor < x { 1 2 }"
    res <- P.parseTestContent P.parseAnonActor input
    shouldBeError res

    let input = "actor { }"
    res <- P.parseTestContent P.parseAnonActor input
    shouldBeError res

  it "parses event expression" $ do
    let input = "on test() => 1"
    let expected = on "test" [] (int 1)
    res <- P.parseTestContent P.parseEvent input
    res `shouldBeRight` expected

    let input = "on test() =>"
    res <- P.parseTestContent P.parseEvent input
    shouldBeError res

    let input = "on test() "
    res <- P.parseTestContent P.parseEvent input
    shouldBeError res

    let input = "on test(x, y) => 1"
    let expected = on "test" ["x", "y"] (int 1)
    res <- P.parseTestContent P.parseEvent input
    res `shouldBeRight` expected

    let input = "on test(x, y) =>"
    res <- P.parseTestContent P.parseEvent input
    shouldBeError res

  it "parses a spawn expression" $ do
    let input = "spawn 1"
    let expected = HLIR.MkExprSpawn (int 1)
    res <- P.parseTestContent P.parseSpawn input
    res `shouldBeRight` expected

    let input = "spawn"
    res <- P.parseTestContent P.parseSpawn input
    shouldBeError res

  it "parses an expression" $ do
    let input = "1"
    let expected = int 1
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "true"
    let expected = bool True
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "false"
    let expected = bool False
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "42"
    let expected = int 42
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "42.0"
    let expected = HLIR.MkExprLiteral $ HLIR.MkLitFloat 42.0
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "'a'"
    let expected = HLIR.MkExprLiteral $ HLIR.MkLitChar 'a'
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "\"hello\""
    let expected = HLIR.MkExprLiteral $ HLIR.MkLitString "hello"
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "x"
    let expected = HLIR.MkExprVariable $ HLIR.MkAnnotation "x" Nothing
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "[]"
    let expected = HLIR.MkExprList []
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "if true then 1 else 0"
    let expected = ifThenElse (bool True) (int 1) (int 0)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "let x = 1"
    let expected = HLIR.MkExprLet (HLIR.MkAnnotation "x" Nothing) (int 1)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "mut x = 1"
    let expected = HLIR.MkExprMut (HLIR.MkAnnotation "x" Nothing) (int 1)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "while true { 1 }"
    let expected = HLIR.MkExprWhile (bool True) (HLIR.MkExprBlock [int 1])
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "{}"
    let expected = HLIR.MkExprBlock []
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "fn f() => 1"
    let expected = function "f" [] (int 1)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "fn f() =>"
    res <- P.parseTestContent P.parseExpression input
    shouldBeError res

    let input = "fn f() "
    res <- P.parseTestContent P.parseExpression input
    shouldBeError res

    let input = "fn fun(x, y) => 1"
    let expected = function "fun" ["x", "y"] (int 1)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "fn fun(x, y) =>"
    res <- P.parseTestContent P.parseExpression input
    shouldBeError res

    let input = "fn (x, y) => 1"
    let expected = lambda ["x", "y"] (int 1)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "fn (x, y) =>"
    res <- P.parseTestContent P.parseExpression input
    shouldBeError res

    let input = "actor test < x { }"
    let expected = actor "test" "x" []
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "actor < x { }"
    let expected = anonActor "x" []
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "on test() => 1"
    res <- P.parseTestContent P.parseExpression input
    shouldBeError res

    let input = "on test() "
    res <- P.parseTestContent P.parseExpression input
    shouldBeError res

    let input = "on test(x, y) => 1"
    res <- P.parseTestContent P.parseExpression input
    shouldBeError res

    let input = "f(x, y, z)"
    let expected = HLIR.MkExprApplication (var "f") [var "x", var "y", var "z"]
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "x.f"
    let expected = HLIR.MkExprApplication (var "f") [var "x"]
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "x.f()"
    let expected = HLIR.MkExprApplication (var "f") [var "x"]
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "x.f(x)"
    let expected = HLIR.MkExprApplication (var "f") [var "x", var "x"]
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "x->f()"
    let expected = HLIR.MkExprSend (var "x") "f" []
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected
    
    let input = "x->f(x)"
    let expected = HLIR.MkExprSend (var "x") "f" [var "x"]
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "x->f(x, y)"
    let expected = HLIR.MkExprSend (var "x") "f" [var "x", var "y"]
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "x->f(x, y"
    res <- P.parseTestContent P.parseExpression input
    shouldBeError res

    let input = "x->f(x, )"
    res <- P.parseTestContent P.parseExpression input
    shouldBeError res

    let input = "x.y.z"
    let expected = HLIR.MkExprApplication (var "z") [HLIR.MkExprApplication (var "y") [var "x"]]
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "x.y.z()"
    let expected = HLIR.MkExprApplication (var "z") [HLIR.MkExprApplication (var "y") [var "x"]]
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "x.y.z(x)"
    let expected = HLIR.MkExprApplication (var "z") [HLIR.MkExprApplication (var "y") [var "x"], var "x"]
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "x.y.z(x, y)"
    let expected = HLIR.MkExprApplication (var "z") [HLIR.MkExprApplication (var "y") [var "x"], var "x", var "y"]
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "x->y(x)->z(y)"
    let expected = HLIR.MkExprSend (HLIR.MkExprSend (var "x") "y" [var "x"]) "z" [var "y"]
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "x(y)(z)"
    let expected = HLIR.MkExprApplication (HLIR.MkExprApplication (var "x") [var "y"]) [var "z"]
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "1 * 2"
    let expected = HLIR.MkExprBinary "*" Nothing (int 1) (int 2)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "1 + 2"
    let expected = HLIR.MkExprBinary "+" Nothing (int 1) (int 2)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "1 - 2"
    let expected = HLIR.MkExprBinary "-" Nothing (int 1) (int 2)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "1 / 2"
    let expected = HLIR.MkExprBinary "/" Nothing (int 1) (int 2)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "1 * 2 + 3"
    let expected = HLIR.MkExprBinary "+" Nothing (HLIR.MkExprBinary "*" Nothing (int 1) (int 2)) (int 3)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "1 + 2 * 3"
    let expected = HLIR.MkExprBinary "+" Nothing (int 1) (HLIR.MkExprBinary "*" Nothing (int 2) (int 3))
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "1 == 2"
    let expected = HLIR.MkExprBinary "==" Nothing (int 1) (int 2)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "1 != 2"
    let expected = HLIR.MkExprBinary "!=" Nothing (int 1) (int 2)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "1 < 2 + 2"
    let expected = HLIR.MkExprBinary "<" Nothing (int 1) (HLIR.MkExprBinary "+" Nothing (int 2) (int 2))
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "1 <= 2 + 2"
    let expected = HLIR.MkExprBinary "<=" Nothing (int 1) (HLIR.MkExprBinary "+" Nothing (int 2) (int 2))
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "1 > 2 && 2 < 3"
    let expected = HLIR.MkExprBinary "&&" Nothing (HLIR.MkExprBinary ">" Nothing (int 1) (int 2)) (HLIR.MkExprBinary "<" Nothing (int 2) (int 3))
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

    let input = "1 || 2 && 2"
    let expected = HLIR.MkExprBinary "&&" Nothing (HLIR.MkExprBinary "||" Nothing (int 1) (int 2)) (int 2)
    res <- P.parseTestContent P.parseExpression input
    res `shouldBeRight` expected

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
    let expected = HLIR.MkExprRequire "hello"
    res <- P.parseTestContent P.parseRequire input
    res `shouldBeRight` expected

  it "parses interface" $ do
    let input = "interface I {}"
    let expected = HLIR.MkExprInterface (HLIR.MkAnnotation "I" []) []
    res <- P.parseTestContent P.parseInterface input
    res `shouldBeRight` expected

    let input = "interface I { fn f(x: int) }"
    let funTy = [HLIR.MkTyInt] HLIR.:->: HLIR.MkTyUnit
    let expected = HLIR.MkExprInterface (HLIR.MkAnnotation "I" []) [
            HLIR.MkAnnotation "f" funTy
          ]
    res <- P.parseTestContent P.parseInterface input
    res `shouldBeRight` expected

    let input = "interface I { fn f(x: int) fn g(x: int) }"
    let funTy = [HLIR.MkTyInt] HLIR.:->: HLIR.MkTyUnit
    let expected = HLIR.MkExprInterface (HLIR.MkAnnotation "I" []) [
            HLIR.MkAnnotation "f" funTy,
            HLIR.MkAnnotation "g" funTy
          ]
    res <- P.parseTestContent P.parseInterface input
    res `shouldBeRight` expected

    let input = "interface test<A> {}"
    let expected = HLIR.MkExprInterface (HLIR.MkAnnotation "test" ["A"]) []
    res <- P.parseTestContent P.parseInterface input
    res `shouldBeRight` expected

    let input = "interface test<A, B> {}"
    let expected = HLIR.MkExprInterface (HLIR.MkAnnotation "test" ["A", "B"]) []
    res <- P.parseTestContent P.parseInterface input
    res `shouldBeRight` expected

  it "parses extern declaration" $ do
    let input = "extern fn f(x: int): int"
    let funTy = [HLIR.MkTyInt] HLIR.:->: HLIR.MkTyInt
    let expected = HLIR.MkExprNative (HLIR.MkAnnotation "f" []) funTy
    res <- P.parseTestContent P.parseExtern input
    res `shouldBeRight` expected

    let input = "extern fn f(x: int)"
    let funTy = [HLIR.MkTyInt] HLIR.:->: HLIR.MkTyUnit
    let expected = HLIR.MkExprNative (HLIR.MkAnnotation "f" []) funTy
    res <- P.parseTestContent P.parseExtern input
    res `shouldBeRight` expected

    let input = "extern fn f(x: int, y: int): string"
    let funTy = [HLIR.MkTyInt, HLIR.MkTyInt] HLIR.:->: HLIR.MkTyString
    let expected = HLIR.MkExprNative (HLIR.MkAnnotation "f" []) funTy
    res <- P.parseTestContent P.parseExtern input
    res `shouldBeRight` expected

    let input = "extern fn f<A>(x: int, y: A): A"
    let funTy = [HLIR.MkTyInt, HLIR.MkTyId "A"] HLIR.:->: HLIR.MkTyId "A"
    let expected = HLIR.MkExprNative (HLIR.MkAnnotation "f" ["A"]) funTy
    res <- P.parseTestContent P.parseExtern input
    res `shouldBeRight` expected

    let input = "extern fn f<A, B>(x: A, y: B): A"
    let funTy = [HLIR.MkTyId "A", HLIR.MkTyId "B"] HLIR.:->: HLIR.MkTyId "A"
    let expected = HLIR.MkExprNative (HLIR.MkAnnotation "f" ["A", "B"]) funTy
    res <- P.parseTestContent P.parseExtern input
    res `shouldBeRight` expected

  it "parses a program" $ do
    let input = "fn main() => 1"
    let expected = [function "main" [] (int 1)]
    res <- P.parseTestContent P.parseProgram input
    res `shouldBeRight` expected

    let input = "fn main() => 1 fn test() => 2"
    let expected = [function "main" [] (int 1), function "test" [] (int 2)]
    res <- P.parseTestContent P.parseProgram input
    res `shouldBeRight` expected

    let input = "fn main() => 1 fn test() => 2 fn test2() => 3"
    let expected = [function "main" [] (int 1), function "test" [] (int 2), function "test2" [] (int 3)]
    res <- P.parseTestContent P.parseProgram input
    res `shouldBeRight` expected

    let input = "fn main() => 1 fn test() => 2 fn test2() => 3 fn test3() => 4"
    let expected = [function "main" [] (int 1), function "test" [] (int 2), function "test2" [] (int 3), function "test3" [] (int 4)]
    res <- P.parseTestContent P.parseProgram input
    res `shouldBeRight` expected

    let input = "fn main() => 1 fn test() => 2 fn test2() => 3 fn test3() => 4 fn test4() => 5"
    let expected = [function "main" [] (int 1), function "test" [] (int 2), function "test2" [] (int 3), function "test3" [] (int 4), function "test4" [] (int 5)]
    res <- P.parseTestContent P.parseProgram input
    res `shouldBeRight` expected

    let input = "actor test < x { }"
    let expected = [actor "test" "x" []]
    res <- P.parseTestContent P.parseProgram input
    res `shouldBeRight` expected

    let input = "actor test < x { } actor test2 < x { }"
    let expected = [actor "test" "x" [], actor "test2" "x" []]
    res <- P.parseTestContent P.parseProgram input
    res `shouldBeRight` expected

    let input = "interface I {}"
    let expected = [HLIR.MkExprInterface (HLIR.MkAnnotation "I" []) []]
    res <- P.parseTestContent P.parseProgram input
    res `shouldBeRight` expected

    let input = "interface I {} interface I2 {}"
    let expected = [HLIR.MkExprInterface (HLIR.MkAnnotation "I" []) [], HLIR.MkExprInterface (HLIR.MkAnnotation "I2" []) []]
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