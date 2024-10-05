{-# LANGUAGE LambdaCase #-}
module Language.Bonzai.Frontend.Module.Resolution where

import qualified Language.Bonzai.Syntax.HLIR as HLIR
import System.FilePath
import qualified GHC.IO as IO
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.Bonzai.Frontend.Parser as P
import qualified Language.Bonzai.Frontend.Parser.Expression as P
import System.Directory (doesFileExist, makeAbsolute)
import Control.Monad.Except
import Control.Monad.Result

data ModuleUnit = MkModuleUnit {
  path :: Text,
  variables :: Set Text,
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

{-# NOINLINE moduleState #-}
moduleState :: IORef ModuleState
moduleState = IO.unsafePerformIO $ newIORef $ MkModuleState "" "" Map.empty

type MonadResolution m = (MonadIO m, MonadError Error m)

resolvePath :: MonadResolution m => FilePath -> m [HLIR.HLIR "expression"]
resolvePath path = do
  st <- readIORef moduleState
  let cwd = st.currentDirectory </> takeDirectory path

  let st' = st { currentDirectory = cwd }

  writeIORef moduleState st'

  let path' = cwd </> takeFileName path <.> "cst"
  let correctPath = normalise path'

  path'' <- liftIO $ makeAbsolute path'

  stack <- readIORef moduleStack

  when (path'' `elem` stack) $ do
    throw $ CyclicModuleDependency path' stack

  modifyIORef' moduleStack (path'' :)

  case Map.lookup path' st.resolved of
    Just m -> return m.expressions
    Nothing -> do
      let m = MkModuleUnit (fromString path') Set.empty [] []
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
          expressions' <- concat <$> mapM resolveImports ast'
          modifyIORef moduleState $ \st'' -> st' {
            resolved = Map.insert path' m { expressions = expressions' } st''.resolved
          }

          modifyIORef' moduleStack $ drop 1

          return expressions'


resolveImports :: MonadResolution m => HLIR.HLIR "expression" -> m [HLIR.HLIR "expression"]
resolveImports (HLIR.MkExprModule {}) = compilerError "Module should not appear in module resolution"
resolveImports (HLIR.MkExprLoc e p) = do
  HLIR.pushPosition p
  e' <- resolveImports e
  void HLIR.popPosition
  return $ HLIR.MkExprLoc <$> e' <*> pure p
resolveImports (HLIR.MkExprRequire path) = resolvePath $ toString path
resolveImports (HLIR.MkExprBlock es) = concat <$> mapM resolveImports es
resolveImports e = return [e]

runModuleResolution :: MonadIO m => [HLIR.HLIR "expression"] -> FilePath -> m (Either Error [HLIR.HLIR "expression"])
runModuleResolution ast path = do
  let st = MkModuleState path (takeDirectory path) Map.empty
  writeIORef moduleState st

  runExceptT $ concat <$> mapM resolveImports ast
