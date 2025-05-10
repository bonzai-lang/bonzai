module Language.Bonzai.Backend.Closure.Hoisting where

import qualified Language.Bonzai.Backend.Closure.Conversion as CC
import qualified Language.Bonzai.Syntax.MLIR as MLIR
import Control.Monad.Result (compilerError)
import qualified Language.Bonzai.Backend.Closure.Free as CC

isClosure :: MLIR.MLIR "expression" -> Bool
isClosure (MLIR.MkExprList [lam, _]) = CC.isLambda lam
isClosure (MLIR.MkExprLoc _ e) = isClosure e
isClosure _ = False

hoist 
  :: MonadIO m 
  => MLIR.MLIR "expression" 
  -> m (MLIR.MLIR "expression", [MLIR.MLIR "expression"])
hoist (MLIR.MkExprEvent es) = do
  name <- CC.freshLambda "hoist"
  (es', hoisted) <- mapAndUnzipM hoist es

  pure (MLIR.MkExprVariable name, MLIR.MkExprLet name (MLIR.MkExprEvent es') : concat hoisted)
hoist (MLIR.MkExprLambda args e) = do
  name <- CC.freshLambda "hoist"
  (e', es) <- hoist e

  pure (MLIR.MkExprVariable name, MLIR.MkExprLet name (MLIR.MkExprLambda args e') : es)
hoist (MLIR.MkExprLet name e) | isClosure e = 
  case CC.removeLoc e of
    MLIR.MkExprList [MLIR.MkExprLambda args body, dict] -> do
      newName <- CC.freshLambda "hoist"
      (dict', hoisted') <- hoist dict
      (body', hoisted) <- hoist body
      let newExpr = MLIR.MkExprList [MLIR.MkExprVariable newName, dict']
      let finalBody = CC.substitute (name, newExpr) body'

      pure (
          MLIR.MkExprLet name (MLIR.MkExprList [MLIR.MkExprVariable newName, dict']), 
          MLIR.MkExprLet newName (MLIR.MkExprLambda args finalBody) : hoisted <> hoisted'
        )
    _ -> compilerError "impossible"
hoist (MLIR.MkExprBlock es) = do
  (es', hoisted) <- mapAndUnzipM hoist es

  pure (MLIR.MkExprBlock es', concat hoisted)
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
hoist (MLIR.MkExprMut e) = do
  (e', hoisted) <- hoist e

  pure (MLIR.MkExprMut e', hoisted)
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
hoist (MLIR.MkExprLoc p e) = do
  (e', hoisted) <- hoist e

  pure (MLIR.MkExprLoc p e', hoisted)
hoist (MLIR.MkExprWhile c e) = do
  (c', hoisted) <- hoist c
  (e', hoisted') <- hoist e

  pure (MLIR.MkExprWhile c' e', hoisted <> hoisted')
hoist MLIR.MkExprSpecial = pure (MLIR.MkExprSpecial, [])
hoist (MLIR.MkExprBinary op e1 e2) = do
  (e1', hoisted) <- hoist e1
  (e2', hoisted') <- hoist e2

  pure (MLIR.MkExprBinary op e1' e2', hoisted <> hoisted')
hoist (MLIR.MkExprRecordAccess e f) = do
  (e', hoisted) <- hoist e

  pure (MLIR.MkExprRecordAccess e' f, hoisted)
hoist (MLIR.MkExprSingleIf c e) = do
  (c', hoisted) <- hoist c
  (e', hoisted') <- hoist e

  pure (MLIR.MkExprSingleIf c' e', hoisted <> hoisted')
hoist (MLIR.MkExprReturn e) = do
  (e', hoisted) <- hoist e

  pure (MLIR.MkExprReturn e', hoisted)
hoist (MLIR.MkExprRecord m) = do
  (hoisted, m') <- mapM swap <$> mapM hoist m

  pure (MLIR.MkExprRecord m', hoisted)

hoistToplevel :: MonadIO m => MLIR.MLIR "expression" -> m [MLIR.MLIR "expression"]
hoistToplevel (MLIR.MkExprLoc p e) = do
  e' <- hoistToplevel e

  pure $ MLIR.MkExprLoc p <$> e'
hoistToplevel (MLIR.MkExprLet n e) | CC.isLambda e = 
  case CC.getLambda e of
    MLIR.MkExprLambda args b -> do
      (b', hoisted) <- hoist b

      pure $ hoisted <> [MLIR.MkExprLet n (MLIR.MkExprLambda args b')]
    _ -> compilerError "impossible"
hoistToplevel (MLIR.MkExprUpdate u e) | CC.isLambda e = do
  case CC.getLambda e of
    MLIR.MkExprLambda args b -> do
      (b', hoisted) <- hoist b

      pure $ hoisted <> [MLIR.MkExprUpdate u (MLIR.MkExprLambda args b')]
    _ -> compilerError "impossible"
hoistToplevel e = do
  (e', hoisted) <- hoist e

  pure $ hoisted <> [e']

runClosureHoisting :: MonadIO m => [MLIR.Expression] -> m [MLIR.Expression]
runClosureHoisting xs = concat <$> mapM hoistToplevel xs