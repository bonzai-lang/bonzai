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
import Text.Megaparsec.Error
import Data.Aeson
import Text.Megaparsec hiding (parseError)
import GHC.Show qualified as Show

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
        ("Cyclic module dependency detected with " <> show (normalise path), Nothing, pos)
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
    UnificationFail got expected ->
      printErrorFromString
        Nothing
        ("Expected " <> toString (toText expected) <> ", but got " <> toString (toText got), Nothing, pos)
        ("Expected type " <> toString (toText expected))

    ActorNotFound name ->
      printErrorFromString
        Nothing
        ("Actor " <> show name <> " not found", Just "check for typo issue with the event name, or missing types in actor header", pos)
        "Resolution"

    NotAnActor ty ->
      printErrorFromString
        Nothing
        ("Expected an actor, but received a " <> show (toText ty), Nothing, pos)
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
    
    InvalidHeader ty ->
      printErrorFromString
        Nothing
        ("Invalid header " <> show (toText ty), Just "try adding explicit annotations", pos)
        "Resolution"

    InvalidUpdate ->
      printErrorFromString
        Nothing
        ("Expected mutable type", Nothing, pos)
        "Resolution"

    UnexpectedRowType ty ->
      printErrorFromString
        Nothing
        ("Unexpected row type " <> show (toText ty), Nothing, pos)
        "Resolution"
    
    CannotInsertLabel label' ->
      printErrorFromString
        Nothing
        ("Cannot insert label " <> show label', Just "check for missing field in the record", pos)
        "Resolution"

type ImportStack = [FilePath]

type Error = (BonzaiError, HLIR.Position)

annotateErrorBundle :: ParseErrorBundle Text Void -> NonEmpty (SourcePos, Text)
annotateErrorBundle bundle
  = fmap (\(err, pos) -> (pos, Text.pack . parseErrorTextPretty $ err)) . fst $
    attachSourcePos errorOffset
                       (bundleErrors bundle)
                       (bundlePosState bundle)

data BonzaiError
  = ParseError P.ParseError
  | CyclicModuleDependency FilePath ImportStack
  | ModuleNotFound FilePath ImportStack
  | VariableNotFound Text
  | CompilerError Text
  | UnificationFail HLIR.Type HLIR.Type
  | ActorNotFound HLIR.Type
  | NotAnActor HLIR.Type
  | EventNotFound Text
  | ExpectedAnActor HLIR.Type
  | InvalidArgumentQuantity Int Int
  | EnvironmentVariableNotFound Text
  | InvalidConstructor Text
  | EmptyMatch
  | InvalidPatternUnion (Set Text) (Set Text)
  | InvalidHeader HLIR.Type
  | InvalidUpdate
  | UnexpectedRowType HLIR.Type
  | CannotInsertLabel Text
  deriving (Eq, Generic)

instance Show BonzaiError where
  show (ParseError e) = showError e
  show (CyclicModuleDependency path []) = "Cyclic module dependency detected with " <> show (normalise path)
  show (CyclicModuleDependency path stack) = "Cyclic module dependency detected with " <> show (normalise path) <> "\nImport stack:\n - "<> intercalate "\n - " (map normalise stack)
  show (ModuleNotFound path stack) = "Module " <> show (normalise path) <> " not found\nImport stack:\n - "<> intercalate "\n - " (map normalise stack)
  show (VariableNotFound name) = "Variable " <> show name <> " not found"
  show (CompilerError msg) = "BONZAI INTERNAL ERROR: " <> show msg
  show (UnificationFail t1 t2) = "Unification failed between " <> toString (toText t1) <> " and " <> toString (toText t2)
  show (ActorNotFound name) = "Actor " <> show name <> " not found"
  show (NotAnActor ty) = "Expected an actor, but received a " <> show (toText ty)
  show (EventNotFound name) = "Event " <> show name <> " not found"
  show (ExpectedAnActor ty) = "Expected an actor, but got " <> show (toText ty)
  show (InvalidArgumentQuantity n k) = "Invalid number of arguments, expected " <> show n <> ", received " <> show k
  show (EnvironmentVariableNotFound name) = "Environment variable " <> show name <> " not found"
  show (InvalidConstructor name) = "Invalid constructor " <> show name
  show EmptyMatch = "Empty match statement"
  show (InvalidPatternUnion env1 env2) = "Invalid pattern union between " <> show env1 <> " and " <> show env2
  show (InvalidHeader ty) = "Invalid header " <> show (toText ty)
  show InvalidUpdate = "Invalid update"
  show (UnexpectedRowType ty) = "Unexpected row type " <> show (toText ty)
  show (CannotInsertLabel label') = "Cannot insert label " <> show label'

instance ToJSON BonzaiError where
  toJSON e = toJSON (show e :: String)

instance ToJSON P.ParseError where
  toJSON e = do
    let res = annotateErrorBundle e
    let res' = (\(pos, err) -> object ["error" .= err, "position" .= pos]) <$> res

    toJSON res'

 
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
