module Main where
import Language.Bonzai.Frontend.Module.Conversion
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
import System.Directory

main :: IO ()
main = do
  args <- getArgs

  file <- case args of
    [x] -> pure x
    _ -> do
      void $ ppError "Usage: bonzai <file>"
      exitFailure

  b <- doesFileExist file
  unless b $ do
    void $ ppError $ "File " <> file <> " does not exist"
    exitFailure

  void . ppBuild $ "Parsing " <> file

  let folder = takeDirectory file
      fileNameWithoutDir = dropExtension $ takeFileName file

  writeIORef moduleState $ 
    MkModuleState 
      fileNameWithoutDir 
      folder
      mempty
      mempty
  moduleResult <- runExceptT $ resolve fileNameWithoutDir True
  
  handle moduleResult . const $ do
    preHLIR <- removeRequires <$> readIORef resultState

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
