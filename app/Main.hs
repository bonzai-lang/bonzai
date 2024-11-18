module Main where
import Language.Bonzai.Frontend.Module.Conversion
import Control.Monad.Result
import Language.Bonzai.Frontend.Typechecking.Checker (runTypechecking)
import Language.Bonzai.Backend.TypeErasure.Conversion (eraseTypes)
import Language.Bonzai.Backend.Closure.Conversion (runClosureConversion)
import Language.Bonzai.Backend.LLIR.Conversion (runLLIRConversion, includeLocations)
import Language.Bonzai.Backend.Closure.Hoisting (runClosureHoisting)
import Language.Bonzai.Backend.ANF.Conversion (runANFConversion)
import Language.Bonzai.Backend.Bytecode.Conversion (runBytecodeConversion)
import Language.Bonzai.Backend.Bytecode.Serialize (runSerializer)

import System.FilePath
import System.Directory
import Options.Applicative

type Typecheck = Bool
type IncludeLocations = Bool

data CLI
  = Build FilePath Bool IncludeLocations

parseBuild :: Parser CLI
parseBuild =
  Build
    <$> argument str (metavar "FILE" <> help "The file to compile")
    <*> switch ( long "content" <> short 'c' <> help "Compile the content instead of a file")
    <*> switch ( long "include-locations" <> short 'i' <> help "Include locations in the output")

parseCLI :: Parser CLI
parseCLI = subparser
  ( command "build" (info parseBuild (progDesc "Compile a Bonzai program"))
  )

buildProgram :: FilePath -> IO ()
buildProgram fp = do
  b <- doesFileExist fp
  unless b $ do
    void $ ppError $ "File " <> fp <> " does not exist"
    exitFailure

  void . ppBuild $ "Parsing " <> fp

  let folder = takeDirectory fp
      fileNameWithoutDir = dropExtension $ takeFileName fp

  writeIORef moduleState $
    MkModuleState
      fileNameWithoutDir
      folder
      mempty
      mempty
      mempty
  moduleResult <- runExceptT $ resolve fileNameWithoutDir True

  handle moduleResult . const $ do
    preHLIR <- removeRequires <$> readIORef resultState

    void . ppBuild $ ("Typechecking the program" :: String)
    typedAST <- runTypechecking preHLIR

    handle typedAST $ \tlir -> do
      ppBuild ("Compiling the program" :: String)
      let mlir = eraseTypes tlir

      closureConverted <- runClosureConversion mlir
      hoistedAST <- runClosureHoisting closureConverted

      anfAST <- runANFConversion hoistedAST

      (llir, cs, gs) <- runLLIRConversion anfAST
      bytecode <- runBytecodeConversion gs llir

      let serialized = runSerializer bytecode cs

      let outputFile = fp <.> "bin"

      writeFileLBS outputFile serialized

      void . ppSuccess $ "Compiled successfully to " <> outputFile

buildProgramFromContent :: String -> IO ()
buildProgramFromContent content = do
  void . ppBuild $ ("Parsing content" :: String)

  let folder = "."
      fileNameWithoutDir = "main"

  writeIORef moduleState $
    MkModuleState
      fileNameWithoutDir
      folder
      mempty
      mempty
      mempty
  moduleResult <- runExceptT $ resolveContent (toText content)

  handle moduleResult . const $ do
    preHLIR <- removeRequires <$> readIORef resultState

    void . ppBuild $ ("Typechecking the program" :: String)
    typedAST <- runTypechecking preHLIR

    handle typedAST $ \_ -> do
      ppSuccess ("Typechecked successfully" :: String)


main :: IO ()
main = do
  cli <- execParser $ info (parseCLI <**> helper) fullDesc

  case cli of
    Build fp False loc -> do
      writeIORef includeLocations loc
      buildProgram fp
    Build content True loc -> do
      writeIORef includeLocations loc
      buildProgramFromContent content