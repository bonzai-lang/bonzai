{-# OPTIONS_GHC -Wno-orphans #-}
module Control.Monad.Result where

import Language.Bonzai.Frontend.Parser qualified as P
import qualified Data.Text as Text
import qualified GHC.IO as IO
import qualified Language.Bonzai.Syntax.HLIR as HLIR
import Control.Monad.Except
import qualified Error.Diagnose as D
import qualified Error.Diagnose.Compat.Megaparsec as D
import qualified Data.Maybe as Mb
import System.Directory (doesFileExist)
import System.FilePath (normalise)
import Control.Color
import Text.Megaparsec.State (PosState(pstateSourcePos))
import Text.Megaparsec.Error (ParseErrorBundle(bundlePosState))

instance (D.HasHints Void String) where
  hints _ = mempty

fromEither :: a -> Either b a -> a
fromEither _ (Right a) = a
fromEither a _ = a

handle :: (MonadIO m) => Either Error b -> (b -> m c) -> m c
handle (Right a) f = f a
handle (Left (err, pos@(p1, _))) _ = liftIO $ do
  case err of
    ParseError e -> parseError e (P.sourceName p1) Nothing
    CyclicModuleDependency path stack ->
      printErrorFromString
        Nothing
        ("Cyclic module dependency detected in " <> show (normalise path), Nothing, pos)
        stackMsg
      where
        stackMsg = "Import stack:\n - "<> intercalate "\n - " (map normalise stack)
    ModuleNotFound path _ ->
      printErrorFromString
        Nothing
        ("Module " <> show (normalise path) <> " not found", Just "check for typo issue with the module name", pos)
        "Resolution"
    VariableNotFound name ->
      printErrorFromString
        Nothing
        ("Variable " <> show name <> " not found", Just "check for typo issue with the variable", pos)
        "Resolution"
    CompilerError msg ->
      printErrorFromString
        Nothing
          ("BONZAI INTERNAL ERROR: " <> show msg, Just "report the issue to Bonzai developers", pos)
          "Resolution"
    UnificationFail t1 t2 ->
      printErrorFromString
        Nothing
        ("Unification failed between " <> toString (toText t1) <> " and " <> toString (toText t2), Nothing, pos)
        "Unification"

    ActorNotFound name ->
      printErrorFromString
        Nothing
        ("Actor " <> show name <> " not found", Just "check for typo issue with the event name, or missing types in actor header", pos)
        "Resolution"

    NotAnActor name ty ->
      printErrorFromString
        Nothing
        ("Variable " <> show name <> " is not an event, but a " <> show (toText ty), Nothing, pos)
        "Typechecking"

    EventNotFound name ->
      printErrorFromString
        Nothing
        ("Event " <> show name <> " not found", Just "check for typo issue with the event name", pos)
        "Resolution"

    ExpectedAnActor ty ->
      printErrorFromString
        (Just "May you have forgotten to define an interface for your actor?")
        ("Expected an actor, but got " <> show (toText ty), Nothing, pos)
        "Typechecking"
    
    InvalidArgumentQuantity n k ->
      printErrorFromString
        Nothing
        ("Invalid number of arguments, expected " <> show n <> ", received " <> show k, Nothing, pos)
        "Resolution"

    EnvironmentVariableNotFound name ->
      printErrorFromString
        Nothing
        ("Environment variable " <> show name <> " not found", Just "check for typo issue with the variable name", pos)
        "Resolution"
    
    InvalidConstructor name ->
      printErrorFromString
        Nothing
        ("Invalid constructor " <> show name, Just "check for typo issue with the constructor name", pos)
        "Resolution"
    
    EmptyMatch ->
      printErrorFromString
        Nothing
        ("Empty match statement", Just "check for missing cases in the match statement", pos)
        "Resolution"
    
    InvalidPatternUnion env1 env2 ->
      printErrorFromString
        Nothing
        ("Invalid pattern union between " <> show env1 <> " and " <> show env2, Nothing, pos)
        "Resolution"


type ImportStack = [FilePath]

type Error = (BonzaiError, HLIR.Position)

data BonzaiError
  = ParseError P.ParseError
  | CyclicModuleDependency FilePath ImportStack
  | ModuleNotFound FilePath ImportStack
  | VariableNotFound Text
  | CompilerError Text
  | UnificationFail HLIR.Type HLIR.Type
  | ActorNotFound HLIR.Type
  | NotAnActor Text HLIR.Type
  | EventNotFound Text
  | ExpectedAnActor HLIR.Type
  | InvalidArgumentQuantity Int Int
  | EnvironmentVariableNotFound Text
  | InvalidConstructor Text
  | EmptyMatch
  | InvalidPatternUnion (Set Text) (Set Text)
  deriving (Show, Eq)

showError :: P.ParseError -> String
showError = P.errorBundlePretty

compilerError :: HasCallStack => Text -> a
compilerError msg = do
  let err = "BONZAI INTERNAL ERROR: " <> msg

  let cs = getCallStack callStack
      callstack = Text.unlines $ map (("    - " <>) . fromString . prettySrcLoc . snd) cs
      pCallstack =
        if null cs
          then ""
          else "\n  A bug occured in Bonzai compiler.\n  CallStack:\n" <> callstack

  IO.unsafePerformIO $ do
    putStrLn . toString $ err <> pCallstack
    exitFailure

throw :: (MonadError Error m, MonadIO m) => BonzaiError -> m a
throw e = do
  pos <- HLIR.popPosition'
  throwError (e, pos)

parseError :: P.ParsingError -> FilePath -> Maybe P.FileContent -> IO a
parseError err' _ fc = do
  let diag :: D.Diagnostic String = D.errorDiagnosticFromBundle Nothing "Parse error on input" Nothing err'

  let fp' = err'.bundlePosState.pstateSourcePos.sourceName

  b <- doesFileExist fp'

  content' <- readFileBS fp'
  let contentAsText = decodeUtf8 content'

  let x' = toString $ if b then contentAsText else Mb.fromJust fc
      diag' = D.addFile diag fp' x'
    in do
      D.printDiagnostic stdout True True 4 D.defaultStyle diag'
      exitFailure

printErrorFromString :: Maybe Text -> (String, Maybe String, HLIR.Position) -> String -> IO a
printErrorFromString content (error', msg, (p1, p2)) step = do
  let p1' = (P.unPos p1.sourceLine, P.unPos p1.sourceColumn)
  let p2' = (P.unPos p2.sourceLine, P.unPos p2.sourceColumn)
  let file' = p1.sourceName
  b <- doesFileExist file'

  content' <- if b then readFileBS file' else pure ""
  let contentAsText = decodeUtf8 content'

  let x' = toString $ if b then contentAsText else Mb.fromJust content
  let pos' = D.Position p1' p2' p1.sourceName
  let beautifulExample = D.err
        Nothing
        error'
        [ (pos', D.This step) ]
        (maybeToList msg)

  -- Create the diagnostic
  let diagnostic  = D.addFile D.def file' x'
  let diagnostic' = D.addReport diagnostic beautifulExample

  -- Print with unicode characters, colors and the default style
  D.printDiagnostic stdout True True 4 D.defaultStyle diagnostic'
  exitFailure

ppError :: ToString a => a -> IO b
ppError t = IO.unsafePerformIO $ do
  putStrLn $ colorBold Red "[error]: " <> toString t

  exitFailure

ppSuccess :: ToString a => a -> IO ()
ppSuccess t = putStrLn $ colorBold Green "[success]: " <> toString t

ppWarning :: ToString a => a -> IO ()
ppWarning t = putStrLn $ colorBold Yellow "[warning]: " <> toString t

ppBuild :: ToString a => a -> IO ()
ppBuild t = putStrLn $ colorBold Cyan "[build]: " <> toString t
