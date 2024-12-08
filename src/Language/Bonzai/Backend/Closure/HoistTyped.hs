{-# LANGUAGE LambdaCase #-}
module Language.Bonzai.Backend.Closure.HoistTyped where

import Language.Bonzai.Syntax.TMLIR qualified as TMLIR
import qualified Language.Bonzai.Backend.Closure.Typed as CC
import Control.Monad.Result (compilerError)

hoistTyped :: MonadIO m => TMLIR.Expression -> m (TMLIR.Expression, [TMLIR.Expression])
hoistTyped (TMLIR.MkExprActor {}) = compilerError "Cannot hoist actor"
hoistTyped (TMLIR.MkExprLambda args ret e) = do
  name <- CC.freshLambda "hoist"

  (e', es) <- hoistTyped e

  pure (TMLIR.MkExprVariable name ret, TMLIR.MkExprLet mempty name ret (TMLIR.MkExprLambda args ret e') : es)
hoistTyped (TMLIR.MkExprLet gens x t (TMLIR.MkExprLambda args ret body)) = do
  (b, hs) <- hoistTyped body
  pure (TMLIR.MkExprLet gens x t (TMLIR.MkExprLambda args ret b), hs)
hoistTyped (TMLIR.MkExprLet gens x t e) = do
  (e', es) <- hoistTyped e
  pure (TMLIR.MkExprLet gens x t e', es)
hoistTyped (TMLIR.MkExprApplication f args t) = do
  (f', hs) <- hoistTyped f
  (args', hs') <- mapAndUnzipM hoistTyped args
  pure (TMLIR.MkExprApplication f' args' t, hs <> concat hs')
hoistTyped (TMLIR.MkExprBlock es t) = do
  (es', hs) <- mapAndUnzipM hoistTyped es
  pure (TMLIR.MkExprBlock es' t, concat hs)
hoistTyped (TMLIR.MkExprOn {}) = compilerError "Cannot hoist on"
hoistTyped (TMLIR.MkExprSend e ev es t) = do
  (e', hs) <- hoistTyped e
  (es', hs') <- mapAndUnzipM hoistTyped es
  pure (TMLIR.MkExprSend e' ev es' t, hs <> concat hs')
hoistTyped (TMLIR.MkExprSpawn e) = do
  (e', hs) <- hoistTyped e
  pure (TMLIR.MkExprSpawn e', hs)
hoistTyped (TMLIR.MkExprList es) = do
  (es', hs) <- mapAndUnzipM hoistTyped es
  pure (TMLIR.MkExprList es', concat hs)
hoistTyped (TMLIR.MkExprTernary c t e ty) = do
  (c', hs) <- hoistTyped c
  (t', hs') <- hoistTyped t
  (e', hs'') <- hoistTyped e
  pure (TMLIR.MkExprTernary c' t' e' ty, hs <> hs' <> hs'')
hoistTyped (TMLIR.MkExprUpdate u e) = do
  (u', hs) <- hoistUpdate u
  (e', hs') <- hoistTyped e
  pure (TMLIR.MkExprUpdate u' e', hs <> hs')
hoistTyped (TMLIR.MkExprWhile c e) = do
  (c', hs) <- hoistTyped c
  (e', hs') <- hoistTyped e
  pure (TMLIR.MkExprWhile c' e', hs <> hs')
hoistTyped (TMLIR.MkExprInterface qvs as) = pure (TMLIR.MkExprInterface qvs as, [])
hoistTyped (TMLIR.MkExprData qvs dcs) = pure (TMLIR.MkExprData qvs dcs, [])
hoistTyped (TMLIR.MkExprField e f) = do
  (e', hs) <- hoistTyped e
  pure (TMLIR.MkExprField e' f, hs)
hoistTyped (TMLIR.MkExprIndex e i) = do
  (e', hs) <- hoistTyped e
  (i', hs') <- hoistTyped i
  pure (TMLIR.MkExprIndex e' i', hs <> hs')
hoistTyped (TMLIR.MkExprUnpack n t e e') = do
  (e'', hs) <- hoistTyped e
  (e''', hs') <- hoistTyped e'
  pure (TMLIR.MkExprUnpack n t e'' e''', hs <> hs')
hoistTyped (TMLIR.MkExprVariable v t) = pure (TMLIR.MkExprVariable v t, [])
hoistTyped (TMLIR.MkExprNative n ty) = pure (TMLIR.MkExprNative n ty, [])
hoistTyped (TMLIR.MkExprLiteral l) = pure (TMLIR.MkExprLiteral l, [])
hoistTyped (TMLIR.MkExprStruct n anns) = do
  res <- mapM (mapM hoistTyped) anns

  let (hss, anns') = unzip $ map (\case 
          (n', (e, es)) -> (es, (n', e))
        ) res
  pure (TMLIR.MkExprStruct n anns', concat hss)
hoistTyped (TMLIR.MkExprRef e t) = do
  (e', hs) <- hoistTyped e
  pure (TMLIR.MkExprRef e' t, hs)
hoistTyped (TMLIR.MkExprUnref e) = do
  (e', hs) <- hoistTyped e
  pure (TMLIR.MkExprUnref e', hs)
hoistTyped (TMLIR.MkExprCast e t) = do
  (e', hs) <- hoistTyped e
  pure (TMLIR.MkExprCast e' t, hs)
hoistTyped (TMLIR.MkExprSizeOf t) = pure (TMLIR.MkExprSizeOf t, [])

hoistUpdate :: MonadIO m => TMLIR.Update -> m (TMLIR.Update, [TMLIR.Expression])
hoistUpdate (TMLIR.MkUpdtVariable a t) = pure (TMLIR.MkUpdtVariable a t, [])
hoistUpdate (TMLIR.MkUpdtField u f) = do
  (u', hs) <- hoistUpdate u
  pure (TMLIR.MkUpdtField u' f, hs)
hoistUpdate (TMLIR.MkUpdtIndex u e) = do
  (u', hs) <- hoistUpdate u
  (e', hs') <- hoistTyped e
  pure (TMLIR.MkUpdtIndex u' e', hs <> hs')
hoistUpdate (TMLIR.MkUpdtUnref u) = do
  (u', hs) <- hoistUpdate u
  pure (TMLIR.MkUpdtUnref u', hs)

hoistToplevel :: MonadIO m => TMLIR.TMLIR "expression" -> m [TMLIR.TMLIR "expression"]
hoistToplevel (TMLIR.MkExprLet g n t (TMLIR.MkExprLambda args ret b)) = do
  (b', hoisted) <- hoistTyped b

  pure $ hoisted <> [TMLIR.MkExprLet g n t (TMLIR.MkExprLambda args ret b')]
hoistToplevel e = do
  (e', hoisted) <- hoistTyped e

  pure $ hoisted <> [e']

runTypedClosureHoisting :: MonadIO m => [TMLIR.TMLIR "expression"] -> m [TMLIR.TMLIR "expression"]
runTypedClosureHoisting = (concat <$>) . mapM hoistToplevel