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
import Language.Bonzai.Frontend.Module.Solver (solveModule)

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

-- | Module state
-- | Used to store the modules units, and some other information about
-- | the modules.
{-# NOINLINE moduleState #-}
moduleState :: IORef ModuleState
moduleState = IO.unsafePerformIO . newIORef $ 
  MkModuleState "" "" Map.empty [] Map.empty

-- | Result state
-- | Used to store result of the resolution, i.e. flattened resolved modules AST.
{-# NOINLINE resultState #-}
resultState :: IORef [HLIR.HLIR "expression"]
resultState = IO.unsafePerformIO . newIORef $ []

type MonadResolution m = (MonadIO m, MonadError Error m)

-- | Get the correct path for a given path, resolving it
-- | to the standard library if needed.
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

-- | Resolve a module from its content, used especially for the LSP server
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
    Right cst' -> do
      ast <- solveModule cst'

      modUnit <- foldlM resolveImports moduleUnit ast
      modifyIORef' resultState (<> ast)
      pure modUnit

-- | Resolve a module from its path
resolve :: (MonadResolution m) => FilePath -> Bool -> m ModuleUnit
resolve path isPublic = do
  st <- readIORef moduleState
  
  -- Get the correct path
  newPath <- getCorrectPath path

  -- Update the module state
  modifyIORef moduleState $ \st' ->
    st'
      { initialPath = newPath
      , currentDirectory = takeDirectory newPath
      }

  -- Get the new module name by making the path relative to the
  -- initial path
  let newModuleName = makeRelative st.initialPath newPath

  -- Checking for file existence
  b <- liftIO $ doesFileExist newPath
  unless b $ throw (ModuleNotFound newPath [])

  case Map.lookup (fromString newModuleName) st.visitStateModules of
    -- If the module is already being visited then we have a cyclic
    -- dependency, because we are trying to visit it again.
    Just Visiting -> throw (CyclicModuleDependency newPath [])

    -- If the module has already been visited, then we can just return
    -- the module unit from the resolved map.
    Just Visited -> do
      case Map.lookup newModuleName st.resolved of
        Just m -> do
          modifyIORef moduleState $ \st' -> 
            st' {
                initialPath= st.initialPath
              , currentDirectory = st.currentDirectory
              }

          pure m { public = isPublic }
        Nothing -> throw (ModuleNotFound newPath [])

    -- If the module has not been visited yet, then we need to visit it.
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

      -- Read the content of the file
      content <- liftIO $ readFileBS newPath
      let contentAsText :: Text = decodeUtf8 content

      -- Parse the content of the file
      cst <- P.parseBonzaiFile newPath contentAsText P.parseProgram

      case cst of
        Left err -> throw (ParseError err)
        Right cst' -> do
          ast <- solveModule cst'

          -- Get the public variables, types, and classes
          let imports = mempty
              variables = getPublicVariables ast
              types = getPublicTypes ast
              classes = mempty

          -- Create a new module unit
          let moduleUnit = MkModuleUnit newModuleName newPath isPublic imports variables types classes

          -- Resolve common errors and resolve imports withing the parsed module
          m' <- foldlM resolveImports moduleUnit ast

          -- Update the module state with the new module unit and with the new 
          -- visit state
          modifyIORef moduleState $ \st' ->
            st
              { resolved = Map.insert newModuleName moduleUnit st'.resolved
              , visitStateModules = 
                  Map.insert 
                    (fromString newModuleName)
                    Visited
                    st'.visitStateModules
              , initialPath = st.initialPath
              , currentDirectory = st.currentDirectory
              }
              
          
          -- Update the result state with the new AST
          modifyIORef' resultState (<> ast)

          -- Return the new module unit, combined with new resolved imports
          pure moduleUnit { imports = m'.imports }

-- | Get the public variables from a list of expressions
-- | This function is used to get the public variables from a list of expressions
-- | in order to store them in the module unit.
getPublicVariables :: [HLIR.HLIR "expression"] -> Set Text
getPublicVariables = foldl' getPublicVariables' mempty
 where
  getPublicVariables' :: Set Text -> HLIR.HLIR "expression" -> Set Text
  getPublicVariables' s (HLIR.MkExprLoc e _) = getPublicVariables' s e
  getPublicVariables' s (HLIR.MkExprPublic (HLIR.MkExprLoc e _)) = getPublicVariables' s (HLIR.MkExprPublic e)
  getPublicVariables' s (HLIR.MkExprPublic (HLIR.MkExprLet _ (Left name) _ _)) = Set.insert name.name s
  getPublicVariables' s (HLIR.MkExprPublic (HLIR.MkExprNative ann _)) = Set.insert ann.name s
  getPublicVariables' s (HLIR.MkExprPublic (HLIR.MkExprData _ cs)) = foldl' getPublicVariablesDataConstr s cs
  getPublicVariables' s _ = s

  getPublicVariablesDataConstr :: Set Text -> HLIR.DataConstructor HLIR.Type -> Set Text
  getPublicVariablesDataConstr s (HLIR.MkDataVariable n) = Set.insert n s
  getPublicVariablesDataConstr s (HLIR.MkDataConstructor n _) = Set.insert n s

-- | Get the public types from a list of expressions
-- | This function is used to get the public types from a list of expressions
-- | in order to store them in the module unit.
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
resolveImports m (HLIR.MkExprApplication f args) = do
  m1 <- resolveImports m f
  foldM' resolveImports m1 args
resolveImports m (HLIR.MkExprLambda args _ body) = do
  old <- readIORef moduleState
  modifyIORef' moduleState $ \st -> 
    st { boundArgs = boundArgs st <> map (.name) args }
  m' <- resolveImports m body
  writeIORef moduleState old

  return m'
resolveImports m (HLIR.MkExprLet _ (Left name) expr b) = do
  let m' = m {variables = Set.singleton name.name <> variables m}
  void $ resolveImports m' expr
  resolveImports m' b
resolveImports _ (HLIR.MkExprLet _ (Right _) _ _) = compilerError "impossible"
resolveImports m (HLIR.MkExprBlock exprs) = do
  void $ foldlM resolveImports m exprs
  pure m
resolveImports m (HLIR.MkExprTernary cond then' else') = do
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
resolveImports m (HLIR.MkExprList es) = foldlM resolveImports m es
resolveImports m (HLIR.MkExprNative ann _) = pure m {variables = Set.singleton ann.name <> variables m}
resolveImports m (HLIR.MkExprMut e) = resolveImports m e
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
resolveImports m (HLIR.MkExprMatch e cs) = do
  void $ resolveImports m e
  mapM_ (\(p, b, _) -> do
      m' <- resolveImportsPattern m p
      resolveImports m' b
    ) cs

  pure m
resolveImports m (HLIR.MkExprPublic e) = resolveImports m e
resolveImports m (HLIR.MkExprModule _ e) = foldM resolveImports m e


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

-- | Check if a variable is defined in a module unit
-- | This algorithm relies on the depth of the search, and the module unit
-- | in which we are looking for the variable.
-- | The depth is used to limit the search to the public variables of the
-- | module unit, and the module unit is used to search for the variable
-- | in the imports of the module unit.
isVariableDefined :: Depth -> Text -> ModuleUnit -> Bool
isVariableDefined depth v m =
  Set.member v m.variables
    || any (isVariableDefined (depth + 1) v) getPublicImports'
 where
  getPublicImports' :: Set ModuleUnit
  getPublicImports' = Set.filter (\m' -> public m' || depth == 0) m.imports

-- | Remove requires from a list of expressions
-- | This function is used to remove requires from a list of expressions
-- | in order to flatten the AST.
removeRequires :: [HLIR.HLIR "expression"] -> [HLIR.HLIR "expression"]
removeRequires = filter (not . isRequire)
 where
  isRequire :: HLIR.HLIR "expression" -> Bool
  isRequire (HLIR.MkExprRequire _ _) = True
  isRequire (HLIR.MkExprPublic e) = isRequire e
  isRequire (HLIR.MkExprLoc e _) = isRequire e
  isRequire _ = False