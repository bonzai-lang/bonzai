module Internal.Utils where
import Test.Hspec
import qualified Language.Bonzai.Syntax.HLIR as HLIR
import Language.Bonzai.Frontend.Parser (parseBonzaiFile, errorBundlePretty, parseBonzaiTestFile)
import Language.Bonzai.Frontend.Parser.Expression (parseProgram)
import System.FilePath (takeFileName, takeDirectory)
import Language.Bonzai.Frontend.Module.Conversion (moduleState, ModuleState (MkModuleState), resolve, resultState, removeRequires)
import Control.Monad.Result (Error)
import Language.Bonzai.Frontend.Typechecking.Monad (CheckerState(MkCheckerState))
import Language.Bonzai.Frontend.Typechecking.Checker (synthesize)
import qualified Language.Bonzai.Frontend.Typechecking.Monad as M
import qualified Data.Map as Map

shouldBeRight' :: (Show b) => Either b (c, HLIR.HLIR "expression") -> HLIR.HLIR "expression" -> Expectation
shouldBeRight' (Right (_, y)) z = 
  if removeLocation y == removeLocation z
  then pure ()
  else expectationFailure $ "Expected " <> show (removeLocation z) <> ",\nbut got  " <> show (removeLocation y)
shouldBeRight' (Left x) _ = expectationFailure $ "Expected Right, but got Left: " <> show x

shouldBeRight :: (Show a, Eq a, Show b) => Either b a -> a -> Expectation
shouldBeRight (Right x) y = x `shouldBe` y
shouldBeRight (Left x) _ = expectationFailure $ "Expected Right, but got Left: " <> show x

shouldBeError :: (Show a, Eq a) => Either b a -> Expectation
shouldBeError (Left _) = pure ()
shouldBeError (Right x) = expectationFailure $ "Expected Left, but got Right " <> show x

ifThenElse :: HLIR.HLIR "expression" -> HLIR.HLIR "expression" -> HLIR.HLIR "expression" -> HLIR.HLIR "expression"
ifThenElse = HLIR.MkExprTernary 

function :: Text -> [Text] -> HLIR.HLIR "expression" -> HLIR.HLIR "expression"
function name args body = HLIR.MkExprLet mempty (Left (HLIR.MkAnnotation name Nothing)) (HLIR.MkExprLambda (map (`HLIR.MkAnnotation` Nothing) args) Nothing body) (HLIR.MkExprVariable (HLIR.MkAnnotation "unit" Nothing))

var :: Text -> HLIR.HLIR "expression"
var = HLIR.MkExprVariable . (`HLIR.MkAnnotation` Nothing)

lambda :: [Text] -> HLIR.HLIR "expression" -> HLIR.HLIR "expression"
lambda args = HLIR.MkExprLambda (map (`HLIR.MkAnnotation` Nothing) args) Nothing

int :: Integer -> HLIR.HLIR "expression"
int = HLIR.MkExprLiteral . HLIR.MkLitInt

bool :: Bool -> HLIR.HLIR "expression"
bool b = HLIR.MkExprLiteral (HLIR.MkLitBool b)

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
  -> IO (Either Error HLIR.Type)
runTypechecking' ast vars = do
  let st = MkCheckerState vars  mempty Nothing mempty
  res <- M.with M.checkerState (const st) $ runExceptT $ traverse synthesize [ast]
  
  case res of
    Left err -> pure $ Left err
    Right [(_, ty)] -> pure $ Right ty
    Right _ -> fail "Expected a single expression, but got multiple"

removeLocation :: HLIR.HLIR "expression" -> HLIR.HLIR "expression"
removeLocation (HLIR.MkExprLoc e _) = removeLocation e
removeLocation (HLIR.MkExprBlock es) = HLIR.MkExprBlock (map removeLocation es)
removeLocation (HLIR.MkExprLet g ann e1 e2) = HLIR.MkExprLet g ann (removeLocation e1) (removeLocation e2)
removeLocation (HLIR.MkExprLambda ann e1 e2) = HLIR.MkExprLambda ann e1 (removeLocation e2)
removeLocation (HLIR.MkExprTernary e1 e2 e3) = HLIR.MkExprTernary (removeLocation e1) (removeLocation e2) (removeLocation e3)
removeLocation (HLIR.MkExprUpdate ann e) = HLIR.MkExprUpdate ann (removeLocation e)
removeLocation (HLIR.MkExprList es) = HLIR.MkExprList $ map removeLocation es
removeLocation (HLIR.MkExprMut e) = HLIR.MkExprMut (removeLocation e)
removeLocation (HLIR.MkExprWhile e1 e2) = HLIR.MkExprWhile (removeLocation e1) (removeLocation e2)
removeLocation (HLIR.MkExprIndex e1 e2) = HLIR.MkExprIndex (removeLocation e1) (removeLocation e2)
removeLocation (HLIR.MkExprMatch e cs) = HLIR.MkExprMatch (removeLocation e) (map (\(p, b, _) -> (removeLocationPattern p, removeLocation b, Nothing)) cs)
removeLocation (HLIR.MkExprRequire path vars) = HLIR.MkExprRequire path vars
removeLocation (HLIR.MkExprApplication f args) = HLIR.MkExprApplication (removeLocation f) (map removeLocation args) 
removeLocation l@(HLIR.MkExprLiteral {}) = l
removeLocation v@(HLIR.MkExprVariable {}) = v
removeLocation n@(HLIR.MkExprNative {}) = n
removeLocation i@(HLIR.MkExprInterface {}) = i
removeLocation d@(HLIR.MkExprData {}) = d
removeLocation (HLIR.MkExprPublic e) = HLIR.MkExprPublic (removeLocation e)
removeLocation (HLIR.MkExprRecordExtension r k opt v) = HLIR.MkExprRecordExtension (removeLocation r) k opt (removeLocation v)
removeLocation (HLIR.MkExprRecordAccess e f) = HLIR.MkExprRecordAccess (removeLocation e) f
removeLocation HLIR.MkExprRecordEmpty = HLIR.MkExprRecordEmpty
removeLocation (HLIR.MkExprSingleIf c e) = HLIR.MkExprSingleIf (removeLocation c) (removeLocation e)
removeLocation (HLIR.MkExprReturn e) = HLIR.MkExprReturn (removeLocation e)
removeLocation HLIR.MkExprBreak = HLIR.MkExprBreak
removeLocation HLIR.MkExprContinue = HLIR.MkExprContinue
removeLocation (HLIR.MkExprSpawn e) = HLIR.MkExprSpawn (removeLocation e)
removeLocation (HLIR.MkExprTypeAlias ann t) = HLIR.MkExprTypeAlias ann t 

removeLocationPattern :: HLIR.HLIR "pattern" -> HLIR.HLIR "pattern"
removeLocationPattern (HLIR.MkPatVariable a t) = HLIR.MkPatVariable a t
removeLocationPattern (HLIR.MkPatLiteral a) = HLIR.MkPatLiteral a
removeLocationPattern (HLIR.MkPatConstructor a b) = HLIR.MkPatConstructor a (map removeLocationPattern b)
removeLocationPattern (HLIR.MkPatList a b t) = HLIR.MkPatList (map removeLocationPattern a) (removeLocationPattern <$> b) t
removeLocationPattern HLIR.MkPatWildcard = HLIR.MkPatWildcard
removeLocationPattern (HLIR.MkPatSpecial a) = HLIR.MkPatSpecial a
removeLocationPattern (HLIR.MkPatCondition a b) = HLIR.MkPatCondition (removeLocation a) (removeLocationPattern b)
removeLocationPattern (HLIR.MkPatLocated p _) = removeLocationPattern p
removeLocationPattern (HLIR.MkPatOr a b) = HLIR.MkPatOr (removeLocationPattern a) (removeLocationPattern b)
removeLocationPattern (HLIR.MkPatRecord m) = HLIR.MkPatRecord (Map.map removeLocationPattern m)
