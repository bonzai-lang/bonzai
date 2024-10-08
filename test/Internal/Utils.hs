module Internal.Utils where
import Test.Hspec
import qualified Language.Bonzai.Syntax.HLIR as HLIR
import Language.Bonzai.Frontend.Parser (parseBonzaiFile, errorBundlePretty)
import Language.Bonzai.Frontend.Parser.Expression (parseProgram)
import System.FilePath (takeFileName, takeDirectory)
import Language.Bonzai.Frontend.Module.Conversion (moduleState, ModuleState (MkModuleState), resolve, resultState, removeRequires)
import Control.Monad.Result (Error)

shouldBeRight :: (Show a, Eq a, Show b) => Either b a -> a -> Expectation
shouldBeRight (Right x) y = x `shouldBe` y
shouldBeRight (Left x) _ = expectationFailure $ "Expected Right, but got Left: " <> show x

shouldBeError :: (Show a, Eq a) => Either b a -> Expectation
shouldBeError (Left _) = pure ()
shouldBeError (Right x) = expectationFailure $ "Expected Left, but got Right " <> show x

ifThenElse :: HLIR.HLIR "expression" -> HLIR.HLIR "expression" -> HLIR.HLIR "expression" -> HLIR.HLIR "expression"
ifThenElse = HLIR.MkExprTernary

on :: Text -> [Text] -> HLIR.HLIR "expression" -> HLIR.HLIR "expression"
on name args = HLIR.MkExprOn name (map (`HLIR.MkAnnotation` Nothing) args)

function :: Text -> [Text] -> HLIR.HLIR "expression" -> HLIR.HLIR "expression"
function name args body = HLIR.MkExprLet (HLIR.MkAnnotation name Nothing) (HLIR.MkExprLambda (map (`HLIR.MkAnnotation` Nothing) args) Nothing body)

var :: Text -> HLIR.HLIR "expression"
var = HLIR.MkExprVariable . (`HLIR.MkAnnotation` Nothing)

lambda :: [Text] -> HLIR.HLIR "expression" -> HLIR.HLIR "expression"
lambda args = HLIR.MkExprLambda (map (`HLIR.MkAnnotation` Nothing) args) Nothing

int :: Integer -> HLIR.HLIR "expression"
int = HLIR.MkExprLiteral . HLIR.MkLitInt

bool :: Bool -> HLIR.HLIR "expression"
bool True = HLIR.MkExprVariable $ HLIR.MkAnnotation "true" Nothing
bool False = HLIR.MkExprVariable $ HLIR.MkAnnotation "false" Nothing

anonActor :: Text -> [HLIR.HLIR "expression"] -> HLIR.HLIR "expression"
anonActor = HLIR.MkExprActor

actor :: Text -> Text -> [HLIR.HLIR "expression"] -> HLIR.HLIR "expression"
actor name arg body = HLIR.MkExprLet (HLIR.MkAnnotation name Nothing) (HLIR.MkExprActor arg body)

fromFile :: FilePath -> IO [HLIR.HLIR "expression"]
fromFile path = do
  content <- readFileBS path
  let contentAsText = decodeUtf8 content

  res <- parseBonzaiFile path contentAsText parseProgram
  case res of
    Left err -> fail $ errorBundlePretty err
    Right ast -> pure ast

runModuleConversion :: FilePath -> IO (Either Error [HLIR.HLIR "expression"])
runModuleConversion path = do
  let fileNameWithoutDir = takeFileName path
      folder = takeDirectory path

  writeIORef moduleState $ 
    MkModuleState 
      fileNameWithoutDir 
      folder
      mempty
      mempty

  moduleResult <- runExceptT $ resolve fileNameWithoutDir True
  
  case moduleResult of 
    Left err -> pure $ Left err
    Right _ -> do
      ret <- Right . removeRequires <$> readIORef resultState
      writeIORef resultState []
      pure ret