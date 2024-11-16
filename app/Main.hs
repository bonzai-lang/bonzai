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
import Language.Bonzai.Syntax.HLIR (programToJSON, jsonToProgram)

import System.FilePath
import System.Directory
import Options.Applicative
import Data.Aeson

type Typecheck = Bool
type IsJSON = Bool

data CLI
  = Build FilePath Bool
  | ToJSON FilePath Typecheck Bool (Maybe FilePath)
  | FromJSON FilePath

parseBuild :: Parser CLI
parseBuild =
  Build
    <$> argument str (metavar "FILE" <> help "The file to compile")
    <*> switch ( long "content" <> short 'c' <> help "Compile the content instead of a file")

parseToJSON :: Parser CLI
parseToJSON =
  ToJSON
    <$> argument str (metavar "FILE" <> help "The file to compile")
    <*> switch
      ( long "typecheck"
      <> short 't'
      <> help "Typecheck the program"
      )
    <*> switch ( long "content" <> short 'c' <> help "Compile the content instead of a file")
    <*> optional (option str ( long "output" <> short 'o' <> metavar "OUTPUT" <> help "Output file for the JSON"))

parseFromJSON :: Parser CLI
parseFromJSON =
  FromJSON
    <$> argument str (metavar "FILE" <> help "The file to compile")

parseCLI :: Parser CLI
parseCLI = subparser
  ( command "build" (info parseBuild (progDesc "Compile a Bonzai program"))
  <> command "to-json"  (info parseToJSON (progDesc "Convert a Bonzai program to JSON"))
  <> command "from-json" (info parseFromJSON (progDesc "Convert a JSON Bonzai program to a binary"))
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

toJSON' :: FilePath -> Typecheck -> Maybe FilePath -> IO ()
toJSON' fp tc out = do
  b <- doesFileExist fp
  unless b $ do
    case out of
      Nothing -> do
        void $ ppError $ "File " <> fp <> " does not exist"
        exitFailure
      Just _ -> do
        putLBSLn $ encode $ object
          [ "error" .= ("File does not exist" :: String)
          ]

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

  case moduleResult of
    Left (error', pos) -> case out of
      Nothing -> case error' of
        ParseError err' -> do
          putLBSLn $ encode err'
        
        _ -> putLBSLn $ encode $ object
          [ "error" .= error'
          , "position" .= pos
          ]

      Just _ -> do
        ppError ("Parsing failed" :: String)
    Right _ -> do
      preHLIR <- removeRequires <$> readIORef resultState

      if tc then do
        typedAST <- runTypechecking preHLIR

        case typedAST of
          Left (error', pos) -> case out of
            Nothing -> do
              putLBSLn $ encode $ object
                [ "error" .= error'
                , "position" .= pos
                ]

            Just _ -> do
              ppError ("Typechecking failed" :: String)
          Right tlir -> do
            putStrLn $ programToJSON tlir
      else case out of
        Nothing -> do
          putStrLn $ programToJSON preHLIR
        Just fp' -> do
          writeFile fp' $ programToJSON preHLIR

fromJSON' :: FilePath -> IO ()
fromJSON' fp = do
  b <- doesFileExist fp
  unless b $ do
    void $ ppError $ "File " <> fp <> " does not exist"
    exitFailure

  void . ppBuild $ "Parsing " <> fp

  json <- decodeUtf8 <$> readFileLBS fp

  let parsed = jsonToProgram json

  case parsed of
    Nothing -> void $ ppError ("Failed to parse JSON" :: String)
    Just ast -> do
      void . ppBuild $ ("Typechecking the program" :: String)
      typedAST <- runTypechecking ast

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

toJSONFromContent :: String -> Typecheck -> Maybe FilePath -> IO ()
toJSONFromContent content tc out = do

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
  
  case moduleResult of
    Left (error', pos) -> case out of
      Nothing ->  case error' of
        ParseError err' -> do
          putLBSLn $ encode err'
        
        _ -> putLBSLn $ encode $ object
          [ "error" .= error'
          , "position" .= pos
          ]

      Just _ -> do
        ppError ("Parsing failed" :: String)
    Right _ -> do
      preHLIR <- removeRequires <$> readIORef resultState
      if tc then do
        typedAST <- runTypechecking preHLIR

        case typedAST of
          Left (error', pos) -> case out of
            Nothing -> do
              putLBSLn $ encode $ object
                [ "error" .= error'
                , "position" .= pos
                ]

            Just _ -> do
              ppError ("Typechecking failed" :: String)
          Right tlir -> do
            putStrLn $ programToJSON tlir
      else case out of
        Nothing -> do
          putStrLn $ programToJSON preHLIR
        Just fp -> do
          writeFile fp $ programToJSON preHLIR


main :: IO ()
main = do
  cli <- execParser $ info (parseCLI <**> helper) fullDesc

  case cli of
    Build fp False -> buildProgram fp
    Build content True -> buildProgramFromContent content

    ToJSON fp tc False output -> toJSON' fp tc output
    ToJSON content tc True output -> toJSONFromContent content tc output

    FromJSON fp -> fromJSON' fp