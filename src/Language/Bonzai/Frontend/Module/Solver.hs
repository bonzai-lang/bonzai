module Language.Bonzai.Frontend.Module.Solver where

import Language.Bonzai.Syntax.HLIR qualified as HLIR
import qualified GHC.IO as IO
import qualified Data.Set as Set
import qualified Data.Text as Text

{-# NOINLINE moduleStack #-}
moduleStack :: IORef (Set Text)
moduleStack = IO.unsafePerformIO $ newIORef Set.empty

{-# NOINLINE renamedVariables #-}
renamedVariables :: IORef (Set Text)
renamedVariables = IO.unsafePerformIO $ newIORef Set.empty

-- | SOLVE MODULE
-- | Solve a module by renaming all the variables in the module with unique names.
solveModule :: MonadIO m => [HLIR.HLIR "expression"] -> m [HLIR.HLIR "expression"]
solveModule (HLIR.MkExprLoc e p : xs) = do
  e' <- solveModule [e]
  xs' <- solveModule xs

  pure ((HLIR.MkExprLoc <$> e' <*> pure p) <> xs')
solveModule (HLIR.MkExprPublic e : xs) = do
  e' <- solveModule [e]
  xs' <- solveModule xs

  pure (HLIR.MkExprPublic <$> e' <> xs')
solveModule (HLIR.MkExprModule n es : xs) = do
  modifyIORef' moduleStack (Set.insert n)
  es' <- mapM solveExpression es
  modifyIORef' moduleStack (Set.delete n)
  
  xs' <- solveModule xs

  pure (es' <> xs')
solveModule (e:es) = do
  e' <- solveExpression e
  es' <- solveModule es

  pure (e' : es')
solveModule [] = pure []

freshSymbol :: MonadIO m => HLIR.Annotation a -> m (HLIR.Annotation a)
freshSymbol ann = do
  stack <- readIORef moduleStack

  pure $ ann { HLIR.name = Text.intercalate "::" (Set.toList stack <> [ann.name]) }

fetchVar :: MonadIO m => HLIR.Annotation a -> m (HLIR.Annotation a)
fetchVar a = do
  vars <- readIORef renamedVariables

  if Set.member a.name vars then freshSymbol a else pure a

-- | SOLVE EXPRESSION
-- | Solve an expression by renaming all the variables in the expression with unique names.
solveExpression :: MonadIO m => HLIR.HLIR "expression" -> m (HLIR.HLIR "expression")
solveExpression (HLIR.MkExprVariable a) = do
  a' <- fetchVar a
  pure $ HLIR.MkExprVariable a'
solveExpression (HLIR.MkExprApplication f args) = do
  f' <- solveExpression f
  args' <- mapM solveExpression args

  pure $ HLIR.MkExprApplication f' args'
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
  a' <- freshSymbol a
  e' <- solveExpression e
  b' <- solveExpression b

  pure $ HLIR.MkExprLet g a' e' b'
solveExpression (HLIR.MkExprMut e) = do
  e' <- solveExpression e

  pure $ HLIR.MkExprMut e' 
solveExpression (HLIR.MkExprBlock es) = do
  es' <- mapM solveExpression es
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
solveExpression e = pure e

-- | SOLVE UPDATE
-- | Solve an update by renaming all the variables in the update with unique names.
solveUpdate :: MonadIO m => HLIR.HLIR "update" -> m (HLIR.HLIR "update")
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
solveCase :: MonadIO m => (HLIR.HLIR "pattern", HLIR.HLIR "expression", HLIR.Position) -> m (HLIR.HLIR "pattern", HLIR.HLIR "expression", HLIR.Position)
solveCase (p, b, ps) = do
  b' <- solveExpression b

  pure (p, b', ps)


