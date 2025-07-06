module Language.Bonzai.Frontend.Module.Solver where

import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.IO qualified as IO
import Language.Bonzai.Syntax.HLIR qualified as HLIR

{-# NOINLINE moduleStack #-}
moduleStack :: IORef (Set Text)
moduleStack = IO.unsafePerformIO $ newIORef Set.empty

{-# NOINLINE renamedVariables #-}
renamedVariables :: IORef (Set Text)
renamedVariables = IO.unsafePerformIO $ newIORef Set.empty

-- | SOLVE MODULE
-- | Solve a module by renaming all the variables in the module with unique names.
solveModule :: (MonadIO m) => [HLIR.HLIR "expression"] -> m [HLIR.HLIR "expression"]
solveModule (HLIR.MkExprLoc e p : xs) = do
  es' <- solveModule (e : xs)

  case es' of
    [HLIR.MkExprMatch e'' cs] -> pure [HLIR.MkExprMatch (HLIR.MkExprLoc e'' p) cs]
    (x : xs'') -> pure (HLIR.MkExprLoc x p : xs'')
    _ -> pure (HLIR.MkExprLoc <$> es' <*> pure p)
solveModule (HLIR.MkExprPublic e : xs) = do
  e' <- solveModule (e : xs)

  pure $ case e' of
    [HLIR.MkExprMatch e'' cs] -> [HLIR.MkExprMatch e'' cs]
    (x : xs') -> [HLIR.MkExprPublic x] <> xs'
    [] -> []
solveModule (HLIR.MkExprLet g a e b : xs) = do
  a' <- case a of
    Left ann -> Left <$> freshSymbol ann
    _ -> pure a
  e' <- solveExpression e
  b' <- solveExpression b

  xs' <- solveModule xs

  case a of
    Left _ -> pure $ HLIR.MkExprLet g a' e' b' : xs'
    Right p -> pure [HLIR.MkExprMatch e' [(p, HLIR.MkExprBlock xs', Nothing)]]
solveModule (x : xs) = do
  x' <- solveExpression x
  xs' <- solveModule xs

  pure (x' : xs')
solveModule [] = pure []

freshSymbol :: (MonadIO m) => HLIR.Annotation a -> m (HLIR.Annotation a)
freshSymbol ann = do
  stack <- readIORef moduleStack

  pure $ ann {HLIR.name = Text.intercalate "::" (Set.toList stack <> [ann.name])}

fetchVar :: (MonadIO m) => HLIR.Annotation a -> m (HLIR.Annotation a)
fetchVar a = do
  vars <- readIORef renamedVariables

  if Set.member a.name vars then freshSymbol a else pure a

solveBlock :: (MonadIO m) => [HLIR.HLIR "expression"] -> m [HLIR.HLIR "expression"]
solveBlock (HLIR.MkExprLet g a e b : xs) = do
  a' <- case a of
    Left ann -> Left <$> freshSymbol ann
    _ -> pure a
  e' <- solveExpression e
  b' <- solveExpression b

  xs' <- solveBlock xs

  case a of
    Left _ -> pure $ HLIR.MkExprLet g a' e' b' : xs'
    Right p -> pure [HLIR.MkExprMatch e' [(p, HLIR.MkExprBlock xs', Nothing)]]
solveBlock (HLIR.MkExprLoc e p : xs) = do
  xs' <- solveBlock (e : xs)

  case xs' of
    [HLIR.MkExprMatch e' cs] -> pure [HLIR.MkExprMatch (HLIR.MkExprLoc e' p) cs]
    (x : xs'') -> pure (HLIR.MkExprLoc x p : xs'')
    _ -> pure (HLIR.MkExprLoc <$> xs' <*> pure p)
solveBlock (x : xs) = do
  x' <- solveExpression x
  xs' <- solveBlock xs

  pure (x' : xs')
solveBlock [] = pure []

getBlock :: HLIR.HLIR "expression" -> Maybe [HLIR.HLIR "expression"]
getBlock (HLIR.MkExprBlock es) = Just es
getBlock (HLIR.MkExprLoc e _) = getBlock e
getBlock _ = Nothing

-- | SOLVE EXPRESSION
-- | Solve an expression by renaming all the variables in the expression with unique names.
solveExpression :: (MonadIO m) => HLIR.HLIR "expression" -> m (HLIR.HLIR "expression")
solveExpression (HLIR.MkExprVariable a) = do
  a' <- fetchVar a
  pure $ HLIR.MkExprVariable a'
solveExpression (HLIR.MkExprApplication f args) = do
  f' <- solveExpression f
  args' <- mapM solveExpression args

  pure $ HLIR.MkExprApplication f' args'
solveExpression (HLIR.MkExprLambda args t body) | Just es <- getBlock body = do
  exprs <- solveBlock es

  pure $ HLIR.MkExprLambda args t (HLIR.MkExprBlock exprs)
solveExpression (HLIR.MkExprLambda args t body) = do
  body' <- solveExpression body

  pure $ HLIR.MkExprLambda args t body'
solveExpression (HLIR.MkExprTernary c t e) = do
  c' <- solveExpression c
  t' <- solveExpression t
  e' <- solveExpression e

  pure $ HLIR.MkExprTernary c' t' e'
solveExpression (HLIR.MkExprUpdate u e) = do
  u' <- solveUpdate u
  e' <- solveExpression e

  pure $ HLIR.MkExprUpdate u' e'
solveExpression (HLIR.MkExprLet g a e b) = do
  a' <- case a of
    Left ann -> Left <$> freshSymbol ann
    _ -> pure a
  e' <- solveExpression e
  b' <- solveExpression b

  pure $ HLIR.MkExprLet g a' e' b'
solveExpression (HLIR.MkExprMut e) = do
  e' <- solveExpression e

  pure $ HLIR.MkExprMut e'
solveExpression (HLIR.MkExprBlock es) = do
  es' <- solveBlock es
  pure $ HLIR.MkExprBlock es'
solveExpression (HLIR.MkExprLiteral l) = pure $ HLIR.MkExprLiteral l
solveExpression (HLIR.MkExprRequire a t) = pure $ HLIR.MkExprRequire a t
solveExpression (HLIR.MkExprLoc e p) = do
  e' <- solveExpression e

  pure $ HLIR.MkExprLoc e' p
solveExpression (HLIR.MkExprList es) = do
  es' <- mapM solveExpression es

  pure $ HLIR.MkExprList es'
solveExpression (HLIR.MkExprNative n ty) = pure $ HLIR.MkExprNative n ty
solveExpression (HLIR.MkExprInterface n t) = pure $ HLIR.MkExprInterface n t
solveExpression (HLIR.MkExprWhile c e) = do
  c' <- solveExpression c
  e' <- solveExpression e

  pure $ HLIR.MkExprWhile c' e'
solveExpression (HLIR.MkExprIndex e i) = do
  e' <- solveExpression e
  i' <- solveExpression i

  pure $ HLIR.MkExprIndex e' i'
solveExpression (HLIR.MkExprMatch e cs) = do
  e' <- solveExpression e
  cs' <- mapM solveCase cs

  pure $ HLIR.MkExprMatch e' cs'
solveExpression (HLIR.MkExprRecordAccess e f) = do
  e' <- solveExpression e

  pure $ HLIR.MkExprRecordAccess e' f
solveExpression HLIR.MkExprRecordEmpty = pure HLIR.MkExprRecordEmpty
solveExpression (HLIR.MkExprRecordExtension t l opt r) = do
  t' <- solveExpression t
  r' <- solveExpression r

  pure $ HLIR.MkExprRecordExtension t' l opt r'
solveExpression HLIR.MkExprBreak = pure HLIR.MkExprBreak
solveExpression HLIR.MkExprContinue = pure HLIR.MkExprContinue
solveExpression (HLIR.MkExprSingleIf c e) = do
  c' <- solveExpression c
  e' <- solveExpression e

  pure $ HLIR.MkExprSingleIf c' e'
solveExpression (HLIR.MkExprPublic e) = do
  e' <- solveExpression e

  pure $ HLIR.MkExprPublic e'
solveExpression (HLIR.MkExprData cs) = pure $ HLIR.MkExprData cs
solveExpression (HLIR.MkExprReturn e) = do
  e' <- solveExpression e

  pure $ HLIR.MkExprReturn e'
solveExpression (HLIR.MkExprSpawn e) = do
  e' <- solveExpression e

  pure $ HLIR.MkExprSpawn e'
solveExpression (HLIR.MkExprTypeAlias ann t) = pure $ HLIR.MkExprTypeAlias ann t

-- | SOLVE UPDATE
-- | Solve an update by renaming all the variables in the update with unique names.
solveUpdate :: (MonadIO m) => HLIR.HLIR "update" -> m (HLIR.HLIR "update")
solveUpdate (HLIR.MkUpdtVariable a) = do
  a' <- freshSymbol a
  pure $ HLIR.MkUpdtVariable a'
solveUpdate (HLIR.MkUpdtField u f) = do
  u' <- solveUpdate u
  pure $ HLIR.MkUpdtField u' f
solveUpdate (HLIR.MkUpdtIndex u e) = do
  u' <- solveUpdate u
  e' <- solveExpression e

  pure $ HLIR.MkUpdtIndex u' e'

-- | SOLVE CASE
-- | Solve a case by renaming all the variables in the case with unique names.
solveCase :: (MonadIO m) => (HLIR.HLIR "pattern", HLIR.HLIR "expression", Maybe HLIR.Position) -> m (HLIR.HLIR "pattern", HLIR.HLIR "expression", Maybe HLIR.Position)
solveCase (p, b, ps) = do
  b' <- solveExpression b

  pure (p, b', ps)
