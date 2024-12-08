module Main where
import Language.Bonzai.Frontend.Module.Conversion
import Control.Monad.Result
import Language.Bonzai.Frontend.Typechecking.Checker (runTypechecking)
import Language.Bonzai.Backend.TypeConversion.Conversion (runTypeConversion)
import Language.Bonzai.Backend.TypeConversion.Actor (runActorConversion)

import System.FilePath
import System.Directory
import Options.Applicative
import Language.Bonzai.Backend.Closure.Typed (runTypedClosureConversion)
import Control.Color (printText)
import Language.Bonzai.Backend.Monomorphization.Conversion (runMonomorphization)
import Language.Bonzai.Backend.ANF.Typed (runANFConversion)
import Language.Bonzai.Backend.Closure.HoistTyped (runTypedClosureHoisting)
import Language.Bonzai.Backend.CLang.Generation (runCLangGeneration, order)
import Language.Bonzai.Backend.CLang.CFG (runCFGConversion)

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
      tmlir <- runTypeConversion tlir
      tmlir' <- runActorConversion tmlir

      -- mapM_ printText tmlir'

      monormophized <- runMonomorphization tmlir'
      closureConverted <- runTypedClosureConversion monormophized
      hoistedAST <- runTypedClosureHoisting closureConverted
      anfAST <- runANFConversion hoistedAST

      mapM_ printText tmlir'

      clang <- runCLangGeneration anfAST
      cfgd <- order <$> runCFGConversion clang

      unlessM (doesDirectoryExist (folder </> "output")) $ 
        createDirectory (folder </> "output")

      copyFile "output/actor.c" (folder </> "output" </> "actor.c")
      copyFile "output/actor.h" (folder </> "output" </> "actor.h")

      let content = "#include \"output/actor.h\"\n" <> toText cfgd

      -- mapM_ printText cfgd
      writeFileText (fp -<.> "c") content

      -- closureConverted <- runClosureConversion tmlir
      -- hoistedAST <- runClosureHoisting closureConverted

      -- anfAST <- runANFConversion hoistedAST

      -- (llir, cs, gs) <- runLLIRConversion anfAST
      -- bytecode <- runBytecodeConversion gs llir

      -- let serialized = runSerializer bytecode cs

      -- let outputFile = fp <.> "bin"

      -- writeFileLBS outputFile serialized

      -- void . ppSuccess $ "Compiled successfully to " <> outputFile

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
    Build fp False _ -> do
      -- writeIORef includeLocations loc
      buildProgram fp
    Build content True _ -> do
      -- writeIORef includeLocations loc
      buildProgramFromContent content