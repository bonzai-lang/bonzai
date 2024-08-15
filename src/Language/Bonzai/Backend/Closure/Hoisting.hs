module Language.Bonzai.Backend.Closure.Hoisting where

import qualified Language.Bonzai.Backend.Closure.Conversion as CC
import qualified Language.Bonzai.Syntax.MLIR as MLIR

hoist 
  :: MonadIO m 
  => MLIR.MLIR "expression" 
  -> m (MLIR.MLIR "expression", [MLIR.MLIR "expression"])
hoist (MLIR.MkExprLambda args e) = do
  name <- CC.freshLambda "hoist"
  (e', es) <- hoist e

  pure (MLIR.MkExprVariable name, MLIR.MkExprLet name (MLIR.MkExprLambda args e') : es)
hoist (MLIR.MkExprBlock es) = do
  (es', hoisted) <- mapAndUnzipM hoist es

  pure (MLIR.MkExprBlock es', concat hoisted)
hoist (MLIR.MkExprEvent es) = do
  (es', hoisted) <- mapAndUnzipM hoist es

  pure (MLIR.MkExprEvent es', concat hoisted)
hoist (MLIR.MkExprOn ev args e) = do
  (e', hoisted) <- hoist e

  pure (MLIR.MkExprOn ev args e', hoisted)
hoist (MLIR.MkExprSend e ev es) = do
  (e', hoisted) <- hoist e
  (es', hoisted') <- mapAndUnzipM hoist es

  pure (MLIR.MkExprSend e' ev es', hoisted <> concat hoisted')
hoist (MLIR.MkExprSpawn e) = do
  (e', hoisted) <- hoist e

  pure (MLIR.MkExprSpawn e', hoisted)
hoist (MLIR.MkExprList es) = do
  (es', hoisted) <- mapAndUnzipM hoist es

  pure (MLIR.MkExprList es', concat hoisted)
hoist (MLIR.MkExprNative n ty) = pure (MLIR.MkExprNative n ty, [])
hoist (MLIR.MkExprIndex e i) = do
  (e', hoisted) <- hoist e
  (i', hoisted') <- hoist i

  pure (MLIR.MkExprIndex e' i', hoisted <> hoisted')
hoist (MLIR.MkExprUnpack x e e') = do
  (e'', hoisted) <- hoist e
  (e''', hoisted') <- hoist e'

  pure (MLIR.MkExprUnpack x e'' e''', hoisted <> hoisted')
hoist (MLIR.MkExprVariable v) = pure (MLIR.MkExprVariable v, [])
hoist (MLIR.MkExprApplication f args) = do
  (f', hoisted) <- hoist f
  (args', hoisted') <- mapAndUnzipM hoist args

  pure (MLIR.MkExprApplication f' args', hoisted <> concat hoisted')
hoist (MLIR.MkExprLet x e) = do
  (e', hoisted) <- hoist e

  pure (MLIR.MkExprLet x e', hoisted)
hoist (MLIR.MkExprMut x e) = do
  (e', hoisted) <- hoist e

  pure (MLIR.MkExprMut x e', hoisted)
hoist (MLIR.MkExprTernary c t e) = do
  (c', hoisted) <- hoist c
  (t', hoisted') <- hoist t
  (e', hoisted'') <- hoist e

  pure (MLIR.MkExprTernary c' t' e', hoisted <> hoisted' <> hoisted'')
hoist (MLIR.MkExprUpdate u e) = do
  (u', hoisted) <- hoistUpdate u
  (e', hoisted') <- hoist e

  pure (MLIR.MkExprUpdate u' e', hoisted <> hoisted')
  where
    hoistUpdate (MLIR.MkUpdtVariable v) = pure (MLIR.MkUpdtVariable v, [])
    hoistUpdate (MLIR.MkUpdtField u' f) = do
      (u'', hoisted) <- hoistUpdate u'

      pure (MLIR.MkUpdtField u'' f, hoisted)
    hoistUpdate (MLIR.MkUpdtIndex u' e') = do
      (u'', hoisted) <- hoistUpdate u'
      (e'', hoisted') <- hoist e'

      pure (MLIR.MkUpdtIndex u'' e'', hoisted <> hoisted')
hoist (MLIR.MkExprLiteral l) = pure (MLIR.MkExprLiteral l, [])

runClosureHoisting :: MonadIO m => [MLIR.Expression] -> m [MLIR.Expression]
runClosureHoisting xs = do
  (xs', hoisted) <- mapAndUnzipM hoist xs

  pure $ concat hoisted <> xs'