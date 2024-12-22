module Internal.Utils where
import Test.Hspec
import qualified Language.Bonzai.Syntax.HLIR as HLIR
import Language.Bonzai.Frontend.Parser (parseBonzaiFile, errorBundlePretty, parseBonzaiTestFile)
import Language.Bonzai.Frontend.Parser.Expression (parseProgram)
import System.FilePath (takeFileName, takeDirectory)
import Language.Bonzai.Frontend.Module.Conversion (moduleState, ModuleState (MkModuleState), resolve, resultState, removeRequires)
import Control.Monad.Result (Error)
import Language.Bonzai.Frontend.Typechecking.Monad (CheckerState(MkCheckerState))
import Language.Bonzai.Frontend.Typechecking.Checker (typecheck)
import qualified Language.Bonzai.Frontend.Typechecking.Monad as M
import qualified Data.Map as Map

shouldBeRight :: (Show a, Eq a, Show b) => Either b a -> a -> Expectation
shouldBeRight (Right x) y = x `shouldBe` y
shouldBeRight (Left x) _ = expectationFailure $ "Expected Right, but got Left: " <> show x

shouldBeError :: (Show a, Eq a) => Either b a -> Expectation
shouldBeError (Left _) = pure ()
shouldBeError (Right x) = expectationFailure $ "Expected Left, but got Right " <> show x

ifThenElse :: HLIR.HLIR "expression" -> HLIR.HLIR "expression" -> HLIR.HLIR "expression" -> HLIR.HLIR "expression"
ifThenElse c t e = HLIR.MkExprTernary c t e Nothing

on :: Text -> [Text] -> HLIR.HLIR "expression" -> HLIR.HLIR "expression"
on name args = HLIR.MkExprOn name (map (`HLIR.MkAnnotation` Nothing) args)

function :: Text -> [Text] -> HLIR.HLIR "expression" -> HLIR.HLIR "expression"
function name args body = HLIR.MkExprLet mempty (HLIR.MkAnnotation name Nothing) (HLIR.MkExprLambda (map (`HLIR.MkAnnotation` Nothing) args) Nothing body)

var :: Text -> HLIR.HLIR "expression"
var = HLIR.MkExprVariable . (`HLIR.MkAnnotation` Nothing)

lambda :: [Text] -> HLIR.HLIR "expression" -> HLIR.HLIR "expression"
lambda args = HLIR.MkExprLambda (map (`HLIR.MkAnnotation` Nothing) args) Nothing

int :: Integer -> HLIR.HLIR "expression"
int = HLIR.MkExprLiteral . HLIR.MkLitInt

bool :: Bool -> HLIR.HLIR "expression"
bool b = HLIR.MkExprLiteral (HLIR.MkLitBool b)

anonActor :: Text -> [HLIR.HLIR "expression"] -> HLIR.HLIR "expression"
anonActor = HLIR.MkExprActor . HLIR.MkTyId

actor :: Text -> Text -> [HLIR.HLIR "expression"] -> HLIR.HLIR "expression"
actor name arg body = HLIR.MkExprLet mempty (HLIR.MkAnnotation name Nothing) (HLIR.MkExprActor (HLIR.MkTyId arg) body)

fromFile :: FilePath -> IO [HLIR.HLIR "expression"]
fromFile path = do
  content <- readFileBS path
  let contentAsText = decodeUtf8 content

  res <- parseBonzaiFile path contentAsText parseProgram
  case res of
    Left err -> fail $ errorBundlePretty err
    Right ast -> pure ast

fromText :: Text -> IO (HLIR.HLIR "expression")
fromText content = do
  res <- parseBonzaiTestFile content parseProgram
  case res of
    Left err -> fail $ errorBundlePretty err
    Right [ast] -> pure ast
    Right _ -> fail "Expected a single expression, but got multiple"

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
      mempty

  moduleResult <- runExceptT $ resolve fileNameWithoutDir True
  
  case moduleResult of 
    Left err -> pure $ Left err
    Right _ -> do
      ret <- Right . removeRequires <$> readIORef resultState
      writeIORef resultState []
      pure ret

runTypechecking' 
  :: HLIR.HLIR "expression" 
  -> Map Text HLIR.Scheme 
  -> Map Text (Map Text HLIR.Type)
  -> IO (Either Error HLIR.Type)
runTypechecking' ast vars interfaces = do
  let st = MkCheckerState vars (Map.mapKeys (,[]) interfaces) mempty mempty
  res <- M.with M.checkerState (const st) $ runExceptT $ traverse typecheck [ast]
  
  case res of
    Left err -> pure $ Left err
    Right [(_, ty)] -> pure $ Right ty
    Right _ -> fail "Expected a single expression, but got multiple"

removeLocation :: HLIR.HLIR "expression" -> HLIR.HLIR "expression"
removeLocation (HLIR.MkExprLoc e _) = removeLocation e
removeLocation (HLIR.MkExprBlock es t) = HLIR.MkExprBlock (map removeLocation es) t
removeLocation (HLIR.MkExprLet ann e1 e2) = HLIR.MkExprLet ann e1 (removeLocation e2)
removeLocation (HLIR.MkExprLambda ann e1 e2) = HLIR.MkExprLambda ann e1 (removeLocation e2)
removeLocation (HLIR.MkExprTernary e1 e2 e3 t) = HLIR.MkExprTernary (removeLocation e1) (removeLocation e2) (removeLocation e3) t
removeLocation (HLIR.MkExprUpdate ann e) = HLIR.MkExprUpdate ann (removeLocation e)
removeLocation (HLIR.MkExprActor ann es) = HLIR.MkExprActor ann (map removeLocation es)
removeLocation (HLIR.MkExprOn n ann e) = HLIR.MkExprOn n ann (removeLocation e)
removeLocation (HLIR.MkExprSend e n es t) = HLIR.MkExprSend (removeLocation e) n (map removeLocation es) t
removeLocation (HLIR.MkExprSpawn e) = HLIR.MkExprSpawn (removeLocation e)
removeLocation (HLIR.MkExprList es) = HLIR.MkExprList $ map removeLocation es
removeLocation (HLIR.MkExprMut e ty) = HLIR.MkExprMut (removeLocation e) ty
removeLocation (HLIR.MkExprWhile e1 e2) = HLIR.MkExprWhile (removeLocation e1) (removeLocation e2)
removeLocation (HLIR.MkExprIndex e1 e2) = HLIR.MkExprIndex (removeLocation e1) (removeLocation e2)
removeLocation (HLIR.MkExprMatch e t cs t') = HLIR.MkExprMatch (removeLocation e) t (map (\(p, b, pos) -> (p, removeLocation b, pos)) cs) t'
removeLocation (HLIR.MkExprRequire _ _) = HLIR.MkExprLiteral $ HLIR.MkLitInt 0
removeLocation (HLIR.MkExprApplication f args t) = HLIR.MkExprApplication (removeLocation f) (map removeLocation args) t
removeLocation l@(HLIR.MkExprLiteral {}) = l
removeLocation v@(HLIR.MkExprVariable {}) = v
removeLocation n@(HLIR.MkExprNative {}) = n
removeLocation i@(HLIR.MkExprInterface {}) = i
removeLocation d@(HLIR.MkExprData {}) = d
removeLocation (HLIR.MkExprLive ann e) = HLIR.MkExprLive ann (removeLocation e)
removeLocation (HLIR.MkExprUnwrapLive e t) = HLIR.MkExprUnwrapLive (removeLocation e) t
removeLocation (HLIR.MkExprWrapLive e t) = HLIR.MkExprWrapLive (removeLocation e) t
removeLocation (HLIR.MkExprPublic e) = HLIR.MkExprPublic (removeLocation e)
removeLocation (HLIR.MkExprTryCatch e n e') = HLIR.MkExprTryCatch (removeLocation e) n (removeLocation e')