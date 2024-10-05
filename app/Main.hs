module Main where
import Language.Bonzai.Frontend.Parser (parseBonzaiFile)
import Language.Bonzai.Frontend.Parser.Expression (parseProgram)
import Language.Bonzai.Frontend.Module.Conversion (runModuleConversion)
import Language.Bonzai.Frontend.Module.Resolution (runModuleResolution)
import Control.Monad.Result
import Language.Bonzai.Frontend.Typechecking.Checker (runTypechecking)
import Language.Bonzai.Backend.TypeErasure.Conversion (eraseTypes)
import Language.Bonzai.Backend.Closure.Conversion (runClosureConversion)
import Language.Bonzai.Backend.LLIR.Conversion (runLLIRConversion)
import Language.Bonzai.Backend.Closure.Hoisting (runClosureHoisting)
import Language.Bonzai.Backend.ANF.Conversion (runANFConversion)
import Language.Bonzai.Backend.Bytecode.Conversion (runBytecodeConversion)
import Language.Bonzai.Backend.Bytecode.Serialize (runSerializer)

import System.FilePath

main :: IO ()
main = do
  args <- getArgs

  file <- case args of
    [x] -> pure x
    _ -> do
      putStrLn "Usage: bonzai <file>"
      exitFailure
  
  content <- readFileBS file
  let contentAsText = decodeUtf8 content

  void . ppBuild $ "Parsing " <> file

  result <- parseBonzaiFile file contentAsText parseProgram
  
  case result of
    Left err -> parseError err file (Just contentAsText)
    Right ast -> do
      moduleConversion <- runModuleConversion ast
      moduleResolution <- runModuleResolution moduleConversion file

      handle moduleResolution $ \preHLIR -> do
        ppBuild "Typechecking the program"
        typedAST <- runTypechecking preHLIR

        handle typedAST $ \tlir -> do
          ppBuild "Compiling the program"
          let mlir = eraseTypes tlir

          closureConverted <- runClosureConversion mlir
          hoistedAST <- runClosureHoisting closureConverted

          anfAST <- runANFConversion hoistedAST

          (llir, cs, gs) <- runLLIRConversion anfAST
          bytecode <- runBytecodeConversion gs llir

          let serialized = runSerializer bytecode cs

          let outputFile = file <.> "bin"

          writeFileLBS outputFile serialized

          void . ppSuccess $ "Compiled successfully to " <> outputFile
