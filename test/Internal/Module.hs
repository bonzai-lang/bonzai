{-# LANGUAGE LambdaCase #-}
module Internal.Module where

import Internal.Utils
import Test.Hspec (Spec, describe, it, shouldSatisfy, expectationFailure)
import qualified Data.List as List
import System.FilePath
import qualified Language.Bonzai.Syntax.HLIR as HLIR

testModuleImport :: Spec
testModuleImport = do
  describe "require resolution" $ do
    it "concat two modules" $ do
      a' <- runModuleConversion "test/data/module/a"

      a_plus_b <- fromFile "test/data/module/a-plus-b.bzi"

      (fmap removeLocation <$> a') `shouldBeRight` (removeLocation <$> a_plus_b)

    it "resolve standard path" $ do
      std <- runModuleConversion "test/data/module/std"

      std_result <- fromFile "test/data/module/std-result.bzi"

      (fmap removeLocation <$> std) `shouldBeRight` (removeLocation <$> List.nub std_result)
    
    it "resolve standard path with environment variable" $ do
      std <- runModuleConversion "std:math"

      stdPath <- fromMaybe "" <$> lookupEnv "BONZAI_PATH"

      std_result <- runModuleConversion (stdPath </> "standard" </> "math")

      if (fmap removeLocation <$> std) == (fmap removeLocation <$> std_result)
        then pure ()
        else expectationFailure $ "Expected " <> show (fmap removeLocation <$> std_result) <> ",\nbut got  " <> show (fmap removeLocation <$> std)

    it "detects cyclic module dependency" $ do
      cyclic <- runModuleConversion "test/data/module/cyclic"

      shouldBeError cyclic

    it "detects module not found" $ do
      notFound <- runModuleConversion "test/data/module/not-found"

      shouldBeError notFound
    
    it "detects variable not found" $ do
      notFound <- runModuleConversion "test/data/module/variable-not-found"

      shouldBeError notFound
    
    it "removes requires" $ do
      noRequires <- runModuleConversion "test/data/module/no-requires"

      noRequires `shouldSatisfy` \case
        Right ast -> not $ any isRequire ast
        _ -> False

isRequire :: HLIR.HLIR "expression" -> Bool
isRequire (HLIR.MkExprRequire _ _) = True
isRequire (HLIR.MkExprLoc e _) = isRequire e
isRequire _ = False