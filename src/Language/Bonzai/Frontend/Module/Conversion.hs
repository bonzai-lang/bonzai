module Language.Bonzai.Frontend.Module.Conversion where

import qualified Language.Bonzai.Syntax.HLIR as HLIR
import qualified GHC.IO as IO
import Control.Monad.Result
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Except
import System.FilePath
import System.Directory (makeAbsolute, doesFileExist)
import qualified Language.Bonzai.Frontend.Parser as P
import qualified Language.Bonzai.Frontend.Parser.Expression as P

type MonadConversion m = (MonadIO m, MonadError Error m)

data VisitState
  = NotVisited
  | Visited
  | Visiting
  deriving (Show, Eq)

-- | Module unit
-- | A module unit is a file (resp. module) that contains a list of imports,
-- | a list of variables, a list of types, and a list of classes.
data ModuleUnit = MkModuleUnit
  { name :: FilePath
  , path :: FilePath
  , public :: Bool
  , imports :: Set ModuleUnit
  , -- Imported data
    variables :: Set Text
  , types :: Set Text
  , classes :: Set Text
  }
  deriving (Show, Eq, Ord)

-- | Module state
data ModuleState = MkModuleState
  { initialPath :: FilePath
  , currentDirectory :: FilePath
  , resolved :: Map FilePath ModuleUnit
  , boundArgs :: [Text]
  , visitStateModules :: Map Text VisitState
  }
  deriving (Show, Eq)

{-# NOINLINE importStack #-}
importStack :: IORef ImportStack
importStack = IO.unsafePerformIO . newIORef $ []

{-# NOINLINE moduleState #-}
moduleState :: IORef ModuleState
moduleState = IO.unsafePerformIO . newIORef $ 
  MkModuleState "" "" Map.empty [] Map.empty

{-# NOINLINE resultState #-}
resultState :: IORef [HLIR.HLIR "expression"]
resultState = IO.unsafePerformIO . newIORef $ []

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

resolveContent :: MonadResolution m => Text -> m ModuleUnit
resolveContent content = do
  let imports = mempty
      variables = mempty
      types = mempty
      classes = mempty

  let moduleUnit = MkModuleUnit "" "" False imports variables types classes

  cst <- P.parseBonzaiFile "" content P.parseProgram

  case cst of
    Left err -> throw (ParseError err)
    Right ast -> do
      modUnit <- foldlM resolveImports moduleUnit ast
      modifyIORef' resultState (<> ast)
      pure modUnit

resolve :: (MonadResolution m) => FilePath -> Bool -> m ModuleUnit
resolve path isPublic = do
  st <- readIORef moduleState
  newPath <- getCorrectPath path

  modifyIORef moduleState $ \st' ->
    st'
      { initialPath = newPath
      , currentDirectory = takeDirectory newPath
      }

  let newModuleName = makeRelative st.initialPath newPath

  b <- liftIO $ doesFileExist newPath
  unless b $ throw (ModuleNotFound newPath [])

  case Map.lookup (fromString newModuleName) st.visitStateModules of
    Just Visiting -> throw (CyclicModuleDependency newPath [])
    Just Visited -> do
      case Map.lookup newModuleName st.resolved of
        Just m -> pure m { public = isPublic }
        Nothing -> throw (ModuleNotFound newPath [])
    _ -> do
      -- Mark the module as being visited
      modifyIORef moduleState $ \st' ->
        st' { 
          visitStateModules = 
            Map.insert 
              (fromString newModuleName) 
              Visiting 
              st'.visitStateModules 
        }

      content <- liftIO $ readFileBS newPath
      let contentAsText :: Text = decodeUtf8 content

      cst <- P.parseBonzaiFile newPath contentAsText P.parseProgram

      case cst of
        Left err -> throw (ParseError err)
        Right ast -> do
          let imports = mempty
              variables = getPublicVariables ast
              types = getPublicTypes ast
              classes = mempty

          let moduleUnit = MkModuleUnit newModuleName newPath isPublic imports variables types classes

          m' <- foldlM resolveImports moduleUnit ast

          modifyIORef moduleState $ \st' ->
            st
              { resolved = Map.insert newModuleName moduleUnit st'.resolved
              , visitStateModules = 
                  Map.insert 
                    (fromString newModuleName)
                    Visited
                    st'.visitStateModules
              }
          modifyIORef' resultState (<> ast)

          pure moduleUnit { imports = m'.imports }

getPublicVariables :: [HLIR.HLIR "expression"] -> Set Text
getPublicVariables = foldl' getPublicVariables' mempty
 where
  getPublicVariables' :: Set Text -> HLIR.HLIR "expression" -> Set Text
  getPublicVariables' s (HLIR.MkExprLoc e _) = getPublicVariables' s e
  getPublicVariables' s (HLIR.MkExprPublic (HLIR.MkExprLoc e _)) = getPublicVariables' s (HLIR.MkExprPublic e)
  getPublicVariables' s (HLIR.MkExprPublic (HLIR.MkExprLet _ name _)) = Set.insert name.name s
  getPublicVariables' s (HLIR.MkExprPublic (HLIR.MkExprLive ann _)) = Set.insert ann.name s
  getPublicVariables' s (HLIR.MkExprPublic (HLIR.MkExprNative ann _)) = Set.insert ann.name s
  getPublicVariables' s (HLIR.MkExprPublic (HLIR.MkExprData _ cs)) = foldl' getPublicVariablesDataConstr s cs
  getPublicVariables' s _ = s

  getPublicVariablesDataConstr :: Set Text -> HLIR.DataConstructor HLIR.Type -> Set Text
  getPublicVariablesDataConstr s (HLIR.MkDataVariable n) = Set.insert n s
  getPublicVariablesDataConstr s (HLIR.MkDataConstructor n _) = Set.insert n s

getPublicTypes :: [HLIR.HLIR "expression"] -> Set Text
getPublicTypes = foldl' getPublicTypes' mempty
 where
  getPublicTypes' :: Set Text -> HLIR.HLIR "expression" -> Set Text
  getPublicTypes' s (HLIR.MkExprLoc e _) = getPublicTypes' s e
  getPublicTypes' s (HLIR.MkExprPublic (HLIR.MkExprLoc e _)) = getPublicTypes' s (HLIR.MkExprPublic e)
  getPublicTypes' s (HLIR.MkExprPublic (HLIR.MkExprData ann _)) = Set.insert ann.name s
  getPublicTypes' s _ = s

foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = return z
foldM' f z (x:xs) = do
  z' <- f z x
  z' `seq` foldM' f z' xs

-- | Main conversion function
resolveImports :: (MonadResolution m) => ModuleUnit -> HLIR.HLIR "expression" -> m ModuleUnit
resolveImports m (HLIR.MkExprApplication f args _) = do
  m1 <- resolveImports m f
  foldM' resolveImports m1 args
resolveImports m (HLIR.MkExprLambda args _ body) = do
  old <- readIORef moduleState
  modifyIORef' moduleState $ \st -> 
    st { boundArgs = boundArgs st <> map (.name) args }
  m' <- resolveImports m body
  writeIORef moduleState old

  return m'
resolveImports m (HLIR.MkExprLet _ name expr) = do
  let m' = m {variables = Set.singleton name.name <> variables m}
  void $ resolveImports m' expr
  pure m'
resolveImports m (HLIR.MkExprLive ann expr) = do
  let m' = m {variables = Set.singleton ann.name <> variables m}
  void $ resolveImports m' expr
  pure m'
resolveImports m (HLIR.MkExprBlock exprs _) = do
  void $ foldlM resolveImports m exprs
  pure m
resolveImports m (HLIR.MkExprTernary cond then' else' _) = do
  m1 <- resolveImports m cond
  m2 <- resolveImports m1 then'
  resolveImports m2 else'
resolveImports m (HLIR.MkExprLoc e pos) = 
  HLIR.pushPosition pos *> resolveImports m e <* HLIR.popPosition
resolveImports m (HLIR.MkExprLiteral _) = pure m
resolveImports m (HLIR.MkExprVariable name) = do
  bound <- (.boundArgs) <$> readIORef moduleState

  unless (isVariableDefined 0 name.name m || name.name `elem` bound) $ do
    throw (VariableNotFound name.name)

  pure m
resolveImports m (HLIR.MkExprInterface name _) = do
  let m' = m {types = Set.singleton name.name <> types m}
  pure m'
resolveImports m (HLIR.MkExprPublic (HLIR.MkExprLoc e _)) = resolveImports m (HLIR.MkExprPublic e)
resolveImports m (HLIR.MkExprPublic (HLIR.MkExprRequire path vars)) = do
  m' <- resolve (toString path) True
  let m'' = if null vars then m' else m' { variables = vars }
  pure $ m {imports = Set.insert m'' m.imports}
resolveImports m (HLIR.MkExprRequire path vars) = do
  m' <- resolve (toString path) False

  let m'' = if null vars then m' else m' { variables = vars }

  pure $ m {imports = Set.insert m'' m.imports}
resolveImports m (HLIR.MkExprUpdate u e) = do
  m1 <- resolveUpdate m u
  resolveImports m1 e
  where
    resolveUpdate :: MonadConversion m => ModuleUnit -> HLIR.Update Maybe HLIR.Type -> m ModuleUnit
    resolveUpdate m' (HLIR.MkUpdtVariable _) = pure m'
    resolveUpdate m' (HLIR.MkUpdtField u' _) = resolveUpdate m' u'
    resolveUpdate m' (HLIR.MkUpdtIndex u' e') = do
      m1 <- resolveUpdate m' u'
      resolveImports m1 e'
resolveImports m (HLIR.MkExprSend e _ e' _) = do
  m1 <- resolveImports m e
  mapM_ (resolveImports m1) e'
  pure m
resolveImports m (HLIR.MkExprActor _ e) = foldM resolveImports m e
resolveImports m (HLIR.MkExprOn _ args e) = do
  old <- readIORef moduleState
  modifyIORef' moduleState $ \st -> 
    st { boundArgs = boundArgs st <> map (.name) args }
  m' <- resolveImports m e
  writeIORef moduleState old

  return m'
resolveImports m (HLIR.MkExprSpawn e) = resolveImports m e
resolveImports m (HLIR.MkExprList es) = foldlM resolveImports m es
resolveImports m (HLIR.MkExprNative ann _) = pure m {variables = Set.singleton ann.name <> variables m}
resolveImports m (HLIR.MkExprMut e _) = resolveImports m e
resolveImports m (HLIR.MkExprWhile c e) = do
  m1 <- resolveImports m c
  resolveImports m1 e
resolveImports m (HLIR.MkExprIndex e e') = do
  m1 <- resolveImports m e
  void $ resolveImports m1 e'
  pure m
resolveImports m (HLIR.MkExprData ann cs) = do
  let m' = m {types = Set.singleton ann.name <> types m}
  foldM' resolveImportsDataConstr m' cs
resolveImports m (HLIR.MkExprMatch e _ cs _) = do
  void $ resolveImports m e
  mapM_ (\(p, b, _) -> do
      m' <- resolveImportsPattern m p
      resolveImports m' b
    ) cs

  pure m
resolveImports m (HLIR.MkExprPublic e) = resolveImports m e
resolveImports _ (HLIR.MkExprUnwrapLive {}) = compilerError "UnwrapLive not implemented"
resolveImports _ (HLIR.MkExprWrapLive {}) = compilerError "WrapLive not implemented"

resolveImportsPattern :: MonadConversion m => ModuleUnit -> HLIR.HLIR "pattern" -> m ModuleUnit
resolveImportsPattern m (HLIR.MkPatVariable n _) = pure m { variables = Set.insert n m.variables }
resolveImportsPattern m (HLIR.MkPatConstructor _ ps) = foldM' resolveImportsPattern m ps
resolveImportsPattern m (HLIR.MkPatLiteral _) = pure m
resolveImportsPattern m HLIR.MkPatWildcard = pure m
resolveImportsPattern m (HLIR.MkPatSpecial _) = pure m
resolveImportsPattern m (HLIR.MkPatLocated p _) = resolveImportsPattern m p
resolveImportsPattern m (HLIR.MkPatOr p p') = do
  m1 <- resolveImportsPattern m p
  resolveImportsPattern m1 p'
resolveImportsPattern m (HLIR.MkPatCondition e p) = do
  m1 <- resolveImportsPattern m p
  resolveImports m1 e
resolveImportsPattern m (HLIR.MkPatList ps slice _) = do
  m1 <- foldM' resolveImportsPattern m ps
  case slice of
    Just p -> resolveImportsPattern m1 p
    Nothing -> pure m1

resolveImportsDataConstr :: MonadConversion m => ModuleUnit -> HLIR.DataConstructor HLIR.Type -> m ModuleUnit
resolveImportsDataConstr m (HLIR.MkDataVariable n) = pure m { variables = Set.insert n m.variables }
resolveImportsDataConstr m (HLIR.MkDataConstructor n _) = pure m { variables = Set.insert n m.variables }

type Depth = Int

isVariableDefined :: Depth -> Text -> ModuleUnit -> Bool
isVariableDefined depth v m =
  Set.member v m.variables
    || any (isVariableDefined (depth + 1) v) getPublicImports'
 where
  getPublicImports' :: Set ModuleUnit
  getPublicImports' = Set.filter (\m' -> public m' || depth == 0) m.imports

removeRequires :: [HLIR.HLIR "expression"] -> [HLIR.HLIR "expression"]
removeRequires = filter (not . isRequire)
 where
  isRequire :: HLIR.HLIR "expression" -> Bool
  isRequire (HLIR.MkExprRequire _ _) = True
  isRequire (HLIR.MkExprPublic e) = isRequire e
  isRequire (HLIR.MkExprLoc e _) = isRequire e
  isRequire _ = False