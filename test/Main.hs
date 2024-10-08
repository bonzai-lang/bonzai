module Main (main) where

import Test.Hspec
import Internal.Parser qualified as Parser
import Internal.Module qualified as Module

main :: IO ()
main = do
  hspec $ do
    Parser.testParser
    Module.testModuleImport