{-# LANGUAGE LambdaCase #-}
module Language.Bonzai.Frontend.Module.Conversion where

import qualified Language.Bonzai.Syntax.HLIR as HLIR
import qualified GHC.IO as IO
import Control.Monad.Result (compilerError)
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set

{-# NOINLINE moduleStack #-}
moduleStack :: IORef [Text]
moduleStack = IO.unsafePerformIO $ newIORef []

pushModule :: MonadIO m => Text -> m ()
pushModule m = modifyIORef moduleStack (m :)

popModule :: MonadIO m => m Text
popModule = atomicModifyIORef moduleStack $ \case
  [] -> compilerError "popModule: empty stack"
  x : xs -> (xs, x)

peekModule :: MonadIO m => m Text
peekModule = readIORef moduleStack >>= \case
  [] -> compilerError "peekModule: empty stack"
  x : _ -> pure x

buildModule :: MonadIO m => Text -> m Text
buildModule m = do
  ms <- readIORef moduleStack
  pure $ Text.intercalate "." (reverse (m : ms))
  
buildVariable :: MonadIO m => Text -> m Text
buildVariable v = do
  mdv <- readIORef moduleDefinedVariables
  ms <- readIORef moduleStack

  case Map.lookup ms mdv of
    Just s -> do
      if Set.member v s
        then buildModule v
        else pure v
    Nothing -> pure v

{-# NOINLINE moduleDefinedVariables #-}
moduleDefinedVariables :: IORef (Map [Text] (Set Text))
moduleDefinedVariables = IO.unsafePerformIO $ newIORef Map.empty

addVariable :: MonadIO m => Text -> m ()
addVariable v = do
  ms <- readIORef moduleStack
  modifyIORef moduleDefinedVariables $ \mdv -> case Map.lookup ms mdv of
    Just s -> Map.insert ms (Set.insert v s) mdv
    Nothing -> Map.insert ms (Set.singleton v) mdv

convertToplevel :: MonadIO m => HLIR.HLIR "expression" -> m [HLIR.HLIR "expression"]
convertToplevel (HLIR.MkExprRequire path) = pure [HLIR.MkExprRequire path]
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
convertModule :: MonadIO m => HLIR.HLIR "expression" -> m (HLIR.HLIR "expression")
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
convertModule (HLIR.MkExprEvent e) = do
  e' <- mapM convertModule e
  pure $ HLIR.MkExprEvent e'
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
convertModule (HLIR.MkExprRequire _) = compilerError "convertModule: require should not appear in module conversion"
convertModule (HLIR.MkExprModule _ _) = compilerError "convertModule: module should not appear in module conversion"

convertUpdate :: MonadIO m => HLIR.HLIR "update" -> m (HLIR.HLIR "update")
convertUpdate (HLIR.MkUpdtVariable a) = pure $ HLIR.MkUpdtVariable a
convertUpdate (HLIR.MkUpdtField u f) = do
  u' <- convertUpdate u
  pure $ HLIR.MkUpdtField u' f
convertUpdate (HLIR.MkUpdtIndex u e) = do
  u' <- convertUpdate u
  e' <- convertModule e

  pure $ HLIR.MkUpdtIndex u' e'

runModuleConversion :: MonadIO m => [HLIR.HLIR "expression"] -> m [HLIR.HLIR "expression"]
runModuleConversion = (concat <$>) . mapM convertToplevel
