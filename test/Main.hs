module Main (main) where

import Test.Hspec
import Internal.Parser qualified as Parser
import Internal.Module qualified as Module
import Internal.Typechecking qualified as Type

main :: IO ()
main = do
  hspec $ do
    Parser.testParser
    Module.testModuleImport
    Type.testTypechecking