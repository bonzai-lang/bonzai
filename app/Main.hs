module Main where

import Control.Monad.Result
import Language.Bonzai.Backend.ANF.Conversion (runANFConversion)
import Language.Bonzai.Backend.Bytecode.Conversion (runBytecodeConversion)
import Language.Bonzai.Backend.Bytecode.Serialize (runSerializer)
import Language.Bonzai.Backend.Closure.Conversion (runClosureConversion)
import Language.Bonzai.Backend.Closure.Hoisting (runClosureHoisting)
import Language.Bonzai.Backend.LLIR.Conversion (includeLocations, runLLIRConversion)
import Language.Bonzai.Backend.TypeErasure.Conversion (eraseTypes)
import Language.Bonzai.Frontend.Module.Conversion
import Language.Bonzai.Frontend.Typechecking.Checker (runTypechecking)
import Options.Applicative
import System.Directory
import System.FilePath

type Typecheck = Bool

type IncludeLocations = Bool

data CLI
  = Build FilePath IncludeLocations

-- | COMMAND PARSING
-- | The following functions define the command line interface for the compiler.
-- | The compiler supports a single command, build, which compiles a Bonzai program.
-- | The build command takes a single argument, the file to compile.
-- | The build command also supports an optional flag, include-locations, which
-- | includes source locations in the output.
parseBuild :: Parser CLI
parseBuild =
  Build
    <$> argument str (metavar "FILE" <> help "The file to compile")
    <*> switch (long "include-locations" <> short 'i' <> help "Include locations in the output")

-- | CLI PARSING
-- | The following function defines the top-level command line interface for
-- | the compiler. The CLI consists of a single subparser, build, which is
-- | defined by the parseBuild function.
parseCLI :: Parser CLI
parseCLI =
  subparser
    ( command "build" (info parseBuild (progDesc "Compile a Bonzai program"))
    )

-- | PROGRAM STACK
-- | This function is the entry point for the compiler. It runs the entire
-- | compilation pipeline and emits a bytecode file at the end.
-- | The pipeline consists of the following steps:
-- |
-- | 1. Resolve the main given module and its dependencies
-- | 2. Remove require statements from the AST
-- | 3. Typecheck the AST
-- | 4. Convert the AST to MLIR (Mid-Level Intermediate Representation)
-- | 5. Closure convert the MLIR
-- | 6. Hoist closures in the MLIR
-- | 7. Convert the MLIR to ANF (A-normal form)
-- | 8. Convert the ANF to LLIR (Low-Level Intermediate Representation)
-- | 9. Convert the LLIR to bytecode
-- | 10. Serialize the bytecode
-- |
-- | The serialized bytecode is then written to a file with the same name as the
-- | input file, but with a .bin extension.
buildProgram :: FilePath -> IO ()
buildProgram fp = do
  -- Checking for file existence
  b <- doesFileExist fp
  unless b $ do
    void $ ppError $ "File " <> fp <> " does not exist"
    exitFailure

  -- Setting up the module state
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

    typedAST <- runTypechecking preHLIR

    handle typedAST $ \tlir -> do
      -- Erasing types from the AST, converting it to MLIR
      let mlir = eraseTypes tlir

      closureConverted <- runClosureConversion mlir
      hoistedAST <- runClosureHoisting closureConverted

      anfAST <- runANFConversion hoistedAST

      (llir, cs, gs) <- runLLIRConversion anfAST
      bytecode <- runBytecodeConversion gs llir

      let serialized = runSerializer bytecode cs

      -- Writing the serialized bytecode to a file
      let outputFile = fp <.> "bin"
      writeFileLBS outputFile serialized

-- | MAIN ENTRY POINT
-- | The following function is the main entry point for the compiler. It parses
-- | the command line arguments and runs the appropriate command.
main :: IO ()
main = do
  cli <- execParser $ info (parseCLI <**> helper) fullDesc

  case cli of
    Build fp loc -> do
      writeIORef includeLocations loc
      buildProgram fp
