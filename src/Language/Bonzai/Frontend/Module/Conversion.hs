{-# LANGUAGE LambdaCase #-}
module Language.Bonzai.Frontend.Module.Conversion where

import qualified Language.Bonzai.Syntax.HLIR as HLIR
import qualified GHC.IO as IO
import Control.Monad.Result
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Except
import System.FilePath
import System.Directory (makeAbsolute, doesFileExist)
import qualified Language.Bonzai.Frontend.Parser as P
import qualified Language.Bonzai.Frontend.Parser.Expression as P
import qualified Data.List as List

type MonadConversion m = (MonadIO m, MonadError Error m)

{-# NOINLINE moduleStackD #-}
moduleStackD :: IORef [Text]
moduleStackD = IO.unsafePerformIO $ newIORef []

pushModule :: MonadIO m => Text -> m ()
pushModule m = modifyIORef moduleStackD (m :)

popModule :: MonadIO m => m Text
popModule = atomicModifyIORef moduleStackD $ \case
  [] -> compilerError "popModule: empty stack"
  x : xs -> (xs, x)

peekModule :: MonadIO m => m Text
peekModule = readIORef moduleStackD >>= \case
  [] -> compilerError "peekModule: empty stack"
  x : _ -> pure x

buildModule :: MonadIO m => Text -> m Text
buildModule m = do
  ms <- readIORef moduleStackD
  pure $ Text.intercalate "::" (reverse ms <> [m])

buildVariable :: MonadConversion m => Text -> m Text
buildVariable v = do
  let v' = Text.splitOn "::" v
  let v'' = bisequence (viaNonEmpty init v', viaNonEmpty last v')

  mdv <- readIORef moduleDefinedVariables
  ms <- readIORef moduleStackD

  case v'' of
    Just (vs, var) | not (null vs) -> case Map.lookup vs mdv of
      Just s | Set.member var s -> pure v
      _ -> throw $ VariableNotFound v

    _ -> case Map.lookup ms mdv of
      Just s -> do
        if Set.member v s
          then buildModule v
          else pure v
      Nothing -> throw $ VariableNotFound v

{-# NOINLINE moduleDefinedVariables #-}
moduleDefinedVariables :: IORef (Map [Text] (Set Text))
moduleDefinedVariables = IO.unsafePerformIO $ newIORef Map.empty

addVariable :: MonadIO m => Text -> m ()
addVariable v = do
  ms <- readIORef moduleStackD
  modifyIORef moduleDefinedVariables $ \mdv -> case Map.lookup ms mdv of
    Just s -> Map.insert ms (Set.insert v s) mdv
    Nothing -> Map.insert ms (Set.singleton v) mdv

mergeVariables :: MonadIO m => Map [Text] (Set Text) -> m ()
mergeVariables mdv = modifyIORef moduleDefinedVariables $ \mdv' -> Map.unionWith (<>) mdv mdv'

data ModuleUnit = MkModuleUnit {
  path :: Text,
  variables :: Map [Text] (Set Text),
  modules :: [ModuleUnit],
  expressions :: [HLIR.HLIR "expression"]
} deriving (Eq)

data ModuleState = MkModuleState
  { initialPath :: FilePath
  , currentDirectory :: FilePath
  , resolved :: Map FilePath ModuleUnit
  }
  deriving (Eq)

{-# NOINLINE moduleStack #-}
moduleStack :: IORef [FilePath]
moduleStack = IO.unsafePerformIO $ newIORef []

local' :: MonadConversion m => m a -> m a
local' m = do
  oldModuleStackD <- readIORef moduleStackD
  oldModuleDefinedVariables <- readIORef moduleDefinedVariables

  writeIORef moduleStackD []
  writeIORef moduleDefinedVariables Map.empty

  a <- m

  writeIORef moduleStackD oldModuleStackD
  writeIORef moduleDefinedVariables oldModuleDefinedVariables

  return a
  

{-# NOINLINE moduleState #-}
moduleState :: IORef ModuleState
moduleState = IO.unsafePerformIO $ newIORef $ MkModuleState "" "" Map.empty

type MonadResolution m = (MonadIO m, MonadError Error m)

getCorrectPath :: MonadResolution m => FilePath -> m FilePath
getCorrectPath ('s':'t':'d':':':path) = do
  standardPath <- lookupEnv "BONZAI_PATH"

  case standardPath of
    Just p -> return $ p </> "standard" </> path <.> "bzi"
    Nothing -> throw $ EnvironmentVariableNotFound "BONZAI_PATH"
getCorrectPath path = do
  st <- readIORef moduleState
  let cwd = st.currentDirectory </> takeDirectory path
  let path' = cwd </> takeFileName path <.> "bzi"
  liftIO $ makeAbsolute path'

resolvePath :: MonadResolution m => FilePath -> m ([HLIR.HLIR "expression"], Map [Text] (Set Text))
resolvePath path = do
  st <- readIORef moduleState

  path' <- getCorrectPath path
  let cwd = takeDirectory path'

  let st' = st { currentDirectory = cwd }

  writeIORef moduleState st'

  let correctPath = normalise path'

  path'' <- liftIO $ makeAbsolute path'

  stack <- readIORef moduleStack

  when (path'' `elem` stack) $ do
    throw $ CyclicModuleDependency path' stack

  modifyIORef' moduleStack (path'' :)

  case Map.lookup path' st.resolved of
    Just m -> return (m.expressions, m.variables)
    Nothing -> do
      let m = MkModuleUnit (fromString path') Map.empty [] []
      modifyIORef moduleState $ \st'' -> st' { resolved = Map.insert correctPath m st''.resolved }

      liftIO (doesFileExist path') >>= \case
        False -> throw (ModuleNotFound correctPath stack)
        True -> pure ()

      content <- readFileBS path'
      let contentAsText = decodeUtf8 content

      ast <- P.parseBonzaiFile correctPath contentAsText P.parseProgram

      case ast of
        Left err -> throw $ ParseError err
        Right ast' -> do
          oldModuleStackD <- readIORef moduleStackD
          oldModuleDefinedVariables <- readIORef moduleDefinedVariables
          
          writeIORef moduleStackD []
          writeIORef moduleDefinedVariables Map.empty

          expressions' <- List.nub . concat <$> mapM convertToplevel ast'

          vars <- readIORef moduleDefinedVariables

          modifyIORef moduleState $ \st'' -> st' {
            resolved = Map.insert path' m { 
              expressions = expressions', 
              variables = vars 
            } st''.resolved
          }

          writeIORef moduleStackD oldModuleStackD
          writeIORef moduleDefinedVariables oldModuleDefinedVariables

          modifyIORef' moduleStack $ drop 1

          return (expressions', vars)

convertToplevel :: MonadConversion m => HLIR.HLIR "expression" -> m [HLIR.HLIR "expression"]
convertToplevel (HLIR.MkExprRequire path) = do
  (es, vars) <- resolvePath $ toString path
  mergeVariables vars
  concat <$> mapM convertToplevel es
convertToplevel (HLIR.MkExprLoc e p) = do
  HLIR.pushPosition p
  e' <- convertToplevel e
  void HLIR.popPosition
  pure (HLIR.MkExprLoc <$> e' <*> pure p)
convertToplevel (HLIR.MkExprModule m e) = do
  pushModule m
  e' <- mapM convertModule e
  void popModule
  pure e'
convertToplevel e = pure <$> convertModule e

-- | Main conversion function
convertModule :: MonadConversion m => HLIR.HLIR "expression" -> m (HLIR.HLIR "expression")
convertModule (HLIR.MkExprMut a e) = do
  a' <- buildModule a.name
  addVariable a.name

  e' <- convertModule e
  pure $ HLIR.MkExprMut (a { HLIR.name = a' }) e'
convertModule (HLIR.MkExprLoc e p) = HLIR.MkExprLoc <$> convertModule e <*> pure p
convertModule (HLIR.MkExprNative n ty) = do
  n' <- buildModule n.name
  addVariable n.name

  pure $ HLIR.MkExprNative n { HLIR.name = n' } ty
convertModule (HLIR.MkExprVariable v) = do
  m <- buildVariable v.name
  pure $ HLIR.MkExprVariable v { HLIR.name = m }
convertModule (HLIR.MkExprLet a e) = do
  a' <- buildModule a.name
  addVariable a.name

  e' <- convertModule e
  pure $ HLIR.MkExprLet (a { HLIR.name = a' }) e'
convertModule (HLIR.MkExprBlock es) = HLIR.MkExprBlock <$> mapM convertModule es
convertModule (HLIR.MkExprLambda as ret e) = do
  e' <- convertModule e
  pure $ HLIR.MkExprLambda as ret e'
convertModule (HLIR.MkExprUpdate u e) = do
  u' <- convertUpdate u
  e' <- convertModule e
  pure $ HLIR.MkExprUpdate u' e'
convertModule (HLIR.MkExprApplication e es) = do
  e' <- convertModule e
  es' <- mapM convertModule es
  pure $ HLIR.MkExprApplication e' es'
convertModule (HLIR.MkExprTernary c t e) = do
  c' <- convertModule c
  t' <- convertModule t
  e' <- convertModule e
  pure $ HLIR.MkExprTernary c' t' e'
convertModule (HLIR.MkExprLiteral l) = pure $ HLIR.MkExprLiteral l
convertModule (HLIR.MkExprActor i e) = do
  e' <- mapM convertModule e
  pure $ HLIR.MkExprActor i e'
convertModule (HLIR.MkExprOn ev as e) = do
  e' <- convertModule e
  pure $ HLIR.MkExprOn ev as e'
convertModule (HLIR.MkExprSend e ev a) = do
  e' <- convertModule e
  a' <- mapM convertModule a
  pure $ HLIR.MkExprSend e' ev a'
convertModule (HLIR.MkExprSpawn e) = do
  e' <- convertModule e
  pure $ HLIR.MkExprSpawn e'
convertModule (HLIR.MkExprList es) = do
  es' <- mapM convertModule es
  pure $ HLIR.MkExprList es'
convertModule (HLIR.MkExprInterface ann defs) = do
  defs' <- mapM convertModuleD defs
  pure $ HLIR.MkExprInterface ann defs'
  where
    convertModuleD (HLIR.MkAnnotation name ty) = do
      a' <- buildModule name
      addVariable name

      pure $ HLIR.MkAnnotation a' ty
convertModule (HLIR.MkExprRequire _) = compilerError "convertModule: require should not appear in module conversion"
convertModule (HLIR.MkExprModule _ _) = compilerError "convertModule: module should not appear in module conversion"

convertUpdate :: MonadConversion m => HLIR.HLIR "update" -> m (HLIR.HLIR "update")
convertUpdate (HLIR.MkUpdtVariable a) = pure $ HLIR.MkUpdtVariable a
convertUpdate (HLIR.MkUpdtField u f) = do
  u' <- convertUpdate u
  pure $ HLIR.MkUpdtField u' f
convertUpdate (HLIR.MkUpdtIndex u e) = do
  u' <- convertUpdate u
  e' <- convertModule e

  pure $ HLIR.MkUpdtIndex u' e'

runModuleConversion :: MonadIO m => [HLIR.HLIR "expression"] -> m (Either Error [HLIR.HLIR "expression"])
runModuleConversion xs = do
  writeIORef moduleStack []
  writeIORef moduleDefinedVariables Map.empty

  runExceptT $ List.nub . concat <$> mapM convertToplevel xs
