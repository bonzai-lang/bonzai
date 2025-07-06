module Internal.Typechecking where

import Internal.Utils hiding (shouldBeRight')
import Test.Hspec
import qualified Language.Bonzai.Frontend.Typechecking.Monad as Ty
import qualified Language.Bonzai.Frontend.Typechecking.Unification as Ty
import qualified Language.Bonzai.Syntax.HLIR as HLIR
import GHC.IO (unsafePerformIO)
import qualified Language.Bonzai.Frontend.Typechecking.Checker as Ty
import Data.Map qualified as Map
import Control.Monad.Result
import Prelude hiding (on, bool)

shouldBeError' :: (Show a, Eq a) => Either (a, HLIR.Position) b -> a -> Expectation
shouldBeError' (Left (a, _)) b = a `shouldBe` b
shouldBeError' (Right _) _ = expectationFailure "Expected an error, but got a success"

shouldTypeBe :: Either a HLIR.Type -> HLIR.Type -> Expectation
shouldTypeBe (Right ty) expected = HLIR.simplify ty `shouldReturn` expected
shouldTypeBe (Left _) _ = expectationFailure "Expected a type, but got an error"

shouldBeRight' :: (Show a, Eq a) => Either a b -> Expectation
shouldBeRight' (Right _) = pure ()
shouldBeRight' (Left x) = expectationFailure $ "Expected Right, but got Left: " <> show x

unbound :: Text -> Int -> HLIR.Type
unbound name lvl = do
  let tv = HLIR.Unbound name lvl
  HLIR.MkTyVar (unsafePerformIO $ newIORef tv)

compressPaths' :: [HLIR.Type] -> IO [HLIR.Type]
compressPaths' = mapM HLIR.simplify

testTypechecking :: Spec
testTypechecking = do
  describe "unification" $ do
    it "unifies two type variables" $ do
      t1 <- Ty.fresh
      t2 <- Ty.fresh

      void . runExceptT $ do
        t1 `Ty.unifiesWith` t2

      compressPaths' [t1, t2] `shouldReturn` [unbound "t69" 0, unbound "t69" 0] 
    
    it "unifies two type variables with a type" $ do
      t1 <- Ty.fresh
      t2 <- Ty.fresh
      t3 <- Ty.fresh

      void . runExceptT $ do
        ([t1] HLIR.:->: t2) `Ty.unifiesWith` ([t3] HLIR.:->: t2)

      compressPaths' [t1, t2, t3] `shouldReturn` [unbound "t72" 0, unbound "t71" 0, unbound "t72" 0]

    it "unifies type variables with a concrete type" $ do
      t1 <- Ty.fresh

      void . runExceptT $ do
        t1 `Ty.unifiesWith` HLIR.MkTyInt

      HLIR.simplify t1 `shouldReturn` HLIR.MkTyInt
      
    it "unifies two type variables with a concrete type" $ do
      t1 <- Ty.fresh
      t2 <- Ty.fresh

      void . runExceptT $ do
        t1 `Ty.unifiesWith` t2
        t1 `Ty.unifiesWith` HLIR.MkTyInt

      compressPaths' [t1, t2] `shouldReturn` [HLIR.MkTyInt, HLIR.MkTyInt]
      
    it "unifies two type variables with a concrete type and a type" $ do
      t1 <- Ty.fresh
      t2 <- Ty.fresh
      t3 <- Ty.fresh

      void . runExceptT $ do
        t1 `Ty.unifiesWith` t2
        t2 `Ty.unifiesWith` HLIR.MkTyInt
        t3 `Ty.unifiesWith` ([t1] HLIR.:->: t2)

      let funTy = [HLIR.MkTyInt] HLIR.:->: HLIR.MkTyInt
      compressPaths' [t1, t2, t3] `shouldReturn` [HLIR.MkTyInt, HLIR.MkTyInt, funTy]
    
    it "does not unify two different types" $ do
      t1 <- Ty.fresh

      res <- runExceptT $ do
        t1 `Ty.unifiesWith` HLIR.MkTyFloat
        t1 `Ty.unifiesWith` HLIR.MkTyInt

      shouldBeError res
  
    it "does not unify a type that occurs itself" $ do
      t1 <- Ty.fresh

      res <- runExceptT $ do
        t1 `Ty.unifiesWith` ([t1] HLIR.:->: HLIR.MkTyInt)

      shouldBeError res

    it "does not unify a type that occurs itself in a list" $ do
      t1 <- Ty.fresh

      res <- runExceptT $ do
        t1 `Ty.unifiesWith` HLIR.MkTyList t1

      shouldBeError res

  -- Instantiation algorithm is pretty straightforward, so I'm not going to test it
  -- Same for generalization and occurs check

  describe "literal typechecking" $ do
    it "typechecks an integer" $ do
      let lit = HLIR.MkExprLiteral (HLIR.MkLitInt 42)
      res <- Ty.runTypechecking [lit]
      res `shouldBe` Right [lit]

    it "typechecks a float" $ do
      let lit = HLIR.MkExprLiteral (HLIR.MkLitFloat 42.0)
      res <- Ty.runTypechecking [lit]
      res `shouldBe` Right [lit]

    it "typechecks a char" $ do
      let lit = HLIR.MkExprLiteral (HLIR.MkLitChar 'a')
      res <- Ty.runTypechecking [lit]
      res `shouldBe` Right [lit]

    it "typechecks a string" $ do
      let lit = HLIR.MkExprLiteral (HLIR.MkLitString "hello")
      res <- Ty.runTypechecking [lit]
      res `shouldBe` Right [lit]

  let env = Map.fromList [
          ("print", HLIR.Forall ["a"] ([HLIR.MkTyQuantified "a"] HLIR.:->: HLIR.MkTyUnit)),
          ("+", HLIR.Forall ["a"] ([HLIR.MkTyQuantified "a", HLIR.MkTyQuantified "a"] HLIR.:->: HLIR.MkTyQuantified "a")),
          ("-", HLIR.Forall ["a"] ([HLIR.MkTyQuantified "a", HLIR.MkTyQuantified "a"] HLIR.:->: HLIR.MkTyQuantified "a")),
          ("const", HLIR.Forall ["a", "b"] ([HLIR.MkTyQuantified "a", HLIR.MkTyQuantified "b"] HLIR.:->: HLIR.MkTyQuantified "a")),
          ("intEq", HLIR.Forall [] ([HLIR.MkTyInt, HLIR.MkTyInt] HLIR.:->: HLIR.MkTyBool)),
          ("unit", HLIR.Forall [] HLIR.MkTyUnit)
        ]

  describe "variable typechecking" $ do
    it "typechecks print" $ do
      let var' = HLIR.MkExprVariable (HLIR.MkAnnotation "print" Nothing)
      res <- runTypechecking' var' env 
      shouldBeRight' res
    
    it "typechecks +" $ do
      let var' = HLIR.MkExprVariable (HLIR.MkAnnotation "+" Nothing)
      res <- runTypechecking' var' env 
      shouldBeRight' res
    
    it "typechecks -" $ do
      let var' = HLIR.MkExprVariable (HLIR.MkAnnotation "-" Nothing)
      res <- runTypechecking' var' env 
      shouldBeRight' res
    
    it "typechecks const" $ do
      let var' = HLIR.MkExprVariable (HLIR.MkAnnotation "const" Nothing)
      res <- runTypechecking' var' env 
      shouldBeRight' res
    
    it "does not typecheck an unknown variable" $ do
      let var' = HLIR.MkExprVariable (HLIR.MkAnnotation "unknown" Nothing)
      res <- runTypechecking' var' env 
      shouldBeError' res (VariableNotFound "unknown")
    
  describe "application typechecking" $ do
    it "typechecks a function application" $ do
      let var' = HLIR.MkExprVariable (HLIR.MkAnnotation "print" Nothing)
      let app = HLIR.MkExprApplication var' [HLIR.MkExprLiteral (HLIR.MkLitInt 42)]
      res <- runTypechecking' app env 
      shouldBeRight' res
    
    it "does not typecheck an unknown function" $ do
      let var' = HLIR.MkExprVariable (HLIR.MkAnnotation "unknown" Nothing)
      let app = HLIR.MkExprApplication var' [HLIR.MkExprLiteral (HLIR.MkLitInt 42)]
      res <- runTypechecking' app env 
      shouldBeError' res (VariableNotFound "unknown")
    
    it "does not typecheck an application with the wrong number of arguments" $ do
      let var' = HLIR.MkExprVariable (HLIR.MkAnnotation "print" Nothing)
      let app = HLIR.MkExprApplication var' []
      res <- runTypechecking' app env 
      shouldBeError res
    
    it "does not typecheck an application with the wrong argument type" $ do
      let var' = HLIR.MkExprVariable (HLIR.MkAnnotation "intEq" Nothing)
      let app = HLIR.MkExprApplication var' [HLIR.MkExprLiteral (HLIR.MkLitChar 'a')]
      res <- runTypechecking' app env 
      shouldBeError res
    
  describe "lambda typechecking" $ do
    it "typechecks a lambda" $ do
      let lam = HLIR.MkExprLambda [HLIR.MkAnnotation "x" Nothing] Nothing (HLIR.MkExprVariable (HLIR.MkAnnotation "x" Nothing))
      res <- runTypechecking' lam env 
      shouldBeRight' res
    
    it "does not typecheck a lambda with an unknown variable" $ do
      let lam = HLIR.MkExprLambda [HLIR.MkAnnotation "x" Nothing] Nothing (HLIR.MkExprVariable (HLIR.MkAnnotation "y" Nothing))
      res <- runTypechecking' lam env 
      shouldBeError' res (VariableNotFound "y")

  describe "let typechecking" $ do
    it "typechecks a let" $ do
      let let' = HLIR.MkExprLet mempty (Left (HLIR.MkAnnotation "x" Nothing)) (HLIR.MkExprLiteral (HLIR.MkLitInt 42)) (HLIR.MkExprVariable (HLIR.MkAnnotation "unit" Nothing))
      res <- runTypechecking' let' env 
      shouldBeRight' res
    
    it "does not typecheck a let with an unknown variable" $ do
      let let' = HLIR.MkExprLet mempty (Left (HLIR.MkAnnotation "x" Nothing) )(HLIR.MkExprVariable (HLIR.MkAnnotation "y" Nothing)) (HLIR.MkExprVariable (HLIR.MkAnnotation "unit" Nothing))
      res <- runTypechecking' let' env 
      shouldBeError' res (VariableNotFound "y")
    
  describe "if-then-else typechecking" $ do
    it "typechecks an if-then-else" $ do
      let if' = ifThenElse (bool True) (HLIR.MkExprLiteral (HLIR.MkLitInt 42)) (HLIR.MkExprLiteral (HLIR.MkLitInt 42))
      res <- runTypechecking' if' env 
      shouldBeRight' res
    
    it "does not typecheck an if-then-else with a non-boolean condition" $ do
      let if' = ifThenElse (HLIR.MkExprLiteral (HLIR.MkLitInt 42)) (HLIR.MkExprLiteral (HLIR.MkLitInt 42)) (HLIR.MkExprLiteral (HLIR.MkLitInt 42))
      res <- runTypechecking' if' env 
      shouldBeError res
    
    it "does not typecheck an if-then-else with different branches" $ do
      let if' = ifThenElse (bool True) (HLIR.MkExprLiteral (HLIR.MkLitInt 42)) (HLIR.MkExprLiteral (HLIR.MkLitFloat 42.0))
      res <- runTypechecking' if' env 
      shouldBeError res
    
  describe "block typechecking" $ do
    it "typechecks a block" $ do
      let block = HLIR.MkExprBlock [HLIR.MkExprLiteral (HLIR.MkLitInt 42), HLIR.MkExprLiteral (HLIR.MkLitInt 42)]
      res <- runTypechecking' block env 
      
      res `shouldTypeBe` HLIR.MkTyInt
    
    it "does not typecheck a block with an unknown variable" $ do
      let block = HLIR.MkExprBlock [HLIR.MkExprVariable (HLIR.MkAnnotation "x" Nothing)]
      res <- runTypechecking' block env 
      shouldBeError' res (VariableNotFound "x")
    
  describe "mutable typechecking" $ do
    it "typechecks a mutable" $ do
      let mut = HLIR.MkExprMut (HLIR.MkExprLiteral (HLIR.MkLitInt 42))
      res <- runTypechecking' mut env 
      shouldBeRight' res
    
    it "does not typecheck a mutable with an unknown variable" $ do
      let mut = HLIR.MkExprMut (HLIR.MkExprVariable (HLIR.MkAnnotation "y" Nothing))
      res <- runTypechecking' mut env 
      shouldBeError' res (VariableNotFound "y")
    
  describe "while typechecking" $ do
    it "typechecks a while" $ do
      let while' = HLIR.MkExprWhile (bool True) (HLIR.MkExprLiteral (HLIR.MkLitInt 42))
      res <- runTypechecking' while' env
      shouldBeRight' res
    
    it "does not typecheck a while with a non-boolean condition" $ do
      let while' = HLIR.MkExprWhile (HLIR.MkExprLiteral (HLIR.MkLitInt 42)) (HLIR.MkExprLiteral (HLIR.MkLitInt 42))
      res <- runTypechecking' while' env
      shouldBeError res
    
  describe "list typechecking" $ do
    it "typechecks a list" $ do
      let list = HLIR.MkExprList [HLIR.MkExprLiteral (HLIR.MkLitInt 42), HLIR.MkExprLiteral (HLIR.MkLitInt 42)]
      res <- runTypechecking' list env 
      res `shouldTypeBe` HLIR.MkTyList HLIR.MkTyInt
    
    it "does not typecheck a list with different types" $ do
      let list = HLIR.MkExprList [HLIR.MkExprLiteral (HLIR.MkLitInt 42), HLIR.MkExprLiteral (HLIR.MkLitFloat 42.0)]
      res <- runTypechecking' list env 
      shouldBeError res
    
  describe "native typechecking" $ do
    it "typechecks a native" $ do
      let native = HLIR.MkExprNative (HLIR.MkAnnotation "print" ["a"]) HLIR.MkTyUnit
      res <- runTypechecking' native env 
      shouldBeRight' res
    
    -- Native functions are likely to be fault-tolerant, so I'm not going to test them further
  
  describe "interface typechecking" $ do
    it "typechecks an interface" $ do
      let interface' = HLIR.MkExprInterface (HLIR.MkAnnotation "interface" []) []
      res <- runTypechecking' interface' env 
      shouldBeRight' res

    -- Interfaces are likely to be fault-tolerant, so I'm not going to test them further

  describe "update typechecking" $ do
    let env' = Map.fromList [
            ("x", HLIR.Forall [] $ HLIR.MkTyMutable HLIR.MkTyInt),
            ("y", HLIR.Forall [] HLIR.MkTyInt)
          ]

    it "typechecks a variable update" $ do
      let update = HLIR.MkExprUpdate (HLIR.MkUpdtVariable (HLIR.MkAnnotation "x" Nothing)) (HLIR.MkExprLiteral (HLIR.MkLitInt 42))
      res <- runTypechecking' update env' 
      shouldBeRight' res
    
    it "does not typecheck an update with an unknown variable" $ do
      let update = HLIR.MkExprUpdate (HLIR.MkUpdtVariable (HLIR.MkAnnotation "x" Nothing)) (HLIR.MkExprVariable (HLIR.MkAnnotation "z" Nothing))
      res <- runTypechecking' update env' 
      shouldBeError' res (VariableNotFound "z")
    
    it "does not typecheck an update with an index that is not an integer" $ do
      let update = HLIR.MkExprUpdate (HLIR.MkUpdtIndex (HLIR.MkUpdtVariable (HLIR.MkAnnotation "x" Nothing)) (HLIR.MkExprLiteral (HLIR.MkLitChar 'a'))) (HLIR.MkExprLiteral (HLIR.MkLitInt 42))
      res <- runTypechecking' update env' 
      shouldBeError res
    
    it "does not typechecking a non-mutable variable" $ do
      let update = HLIR.MkExprUpdate (HLIR.MkUpdtVariable (HLIR.MkAnnotation "y" Nothing)) (HLIR.MkExprLiteral (HLIR.MkLitInt 42))
      res <- runTypechecking' update env' 
      shouldBeError res
    
    it "does not typecheck an update with a type mismatch" $ do
      let update = HLIR.MkExprUpdate (HLIR.MkUpdtVariable (HLIR.MkAnnotation "x" Nothing)) (HLIR.MkExprLiteral (HLIR.MkLitFloat 42.0))
      res <- runTypechecking' update env' 
      shouldBeError res
  
    