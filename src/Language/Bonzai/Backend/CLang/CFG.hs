{-# LANGUAGE LambdaCase #-}
module Language.Bonzai.Backend.CLang.CFG where

import Language.Bonzai.Syntax.CLang qualified as CL
import Control.Monad.Result (compilerError)
import qualified GHC.IO as IO
import Language.Bonzai.Backend.CLang.Generation (unsnoc)

{-# NOINLINE counter #-}
counter :: IORef Int
counter = IO.unsafePerformIO $ newIORef 0

freshVar :: MonadIO m => Text -> m Text
freshVar prefix = do
  i <- readIORef counter
  modifyIORef' counter (+1)

  pure $ prefix <> show i

isPanic :: CL.Expression -> Bool
isPanic (CL.MkExprApplication (CL.MkExprVariable "panic") _) = True
isPanic _ = False

cfg :: MonadIO m => CL.Expression -> m (CL.Expression, [CL.Statement])
cfg (CL.MkExprLiteral l) = pure (CL.MkExprLiteral l, [])
cfg (CL.MkExprVariable v) = pure (CL.MkExprVariable v, [])
cfg (CL.MkExprApplication e es) = do
  (e', hs) <- cfg e
  (es', hs') <- mapAndUnzipM cfg es
  pure (CL.MkExprApplication e' es', hs <> concat hs')
cfg (CL.MkExprIndex e i) = do
  (e', hs) <- cfg e
  (i', hs') <- cfg i
  pure (CL.MkExprIndex e' i', hs <> hs')
cfg (CL.MkExprField e f) = do
  (e', hs) <- cfg e
  pure (CL.MkExprField e' f, hs)
cfg (CL.MkExprUpdate u e) = do
  (u', hs) <- cfgUpdate u
  (e', hs') <- cfg e
  pure (CL.MkExprUpdate u' e', hs <> hs')
cfg (CL.MkExprBlock es t) = do
  case unsnoc es of
    Just (es', CL.MkStmtExpr (CL.MkExprApplication (CL.MkExprVariable "panic") args)) -> do
      es'' <- concat <$> mapM cfgStatement es'
      (args', hs) <- mapAndUnzipM cfg args

      pure (CL.MkExprVariable "", es'' <> concat hs <> [CL.MkStmtExpr (CL.MkExprApplication (CL.MkExprVariable "panic") args')])

    Just (es', CL.MkStmtExpr e) | t == "void" -> do
      es'' <- concat <$> mapM cfgStatement es'
      (e', hs) <- cfg e
      pure (CL.MkExprVariable "", es'' <> hs <> [CL.MkStmtExpr e'])

    Just (es', CL.MkStmtExpr e) -> do
      (e', hs) <- cfg e
      name <- freshVar "block"
      let decl = CL.MkStmtDeclare name t

      es'' <- concat <$> mapM cfgStatement es'

      pure (CL.MkExprVariable name, [decl] <> hs <> es'' <> [CL.MkStmtAssign (CL.MkExprVariable name) e'])
    Just _ -> compilerError "Invalid block: not an expression at the end"
    Nothing -> compilerError "Empty block"
cfg (CL.MkExprList es) = do
  (es', hs) <- mapAndUnzipM cfg es
  pure (CL.MkExprList es', concat hs)
cfg (CL.MkExprTernary c t e ty) = do
  (c', hs) <- cfg c
  (t', hs') <- cfg t
  (e', hs'') <- cfg e

  name <- freshVar "ternary"
  let decl = CL.MkStmtDeclare name ty

  let then'
        | isPanic t' = [CL.MkStmtExpr t']
        | ty == "void" = [CL.MkStmtExpr t']
        | otherwise = [CL.MkStmtAssign (CL.MkExprVariable name) t']
  let else'
        | isPanic e' = [CL.MkStmtExpr e']
        | ty == "void" = [CL.MkStmtExpr e']
        | otherwise = [CL.MkStmtAssign (CL.MkExprVariable name) e']

  let if' = CL.MkStmtIf c' (hs' <> then') (hs'' <> else')

  pure (if ty == "void" then CL.MkExprVariable "" else CL.MkExprVariable name, hs <> [decl | ty /= "void"] <> [if'])
cfg (CL.MkExprStruct n anns) = do
  res <- mapM (mapM cfg) anns

  let (hss, anns') = unzip $ map (\case
          (n', (e, hs)) -> (hs, (n', e))
        ) res
  pure (CL.MkExprStruct n anns', concat hss)
cfg (CL.MkExprRef e) = do
  (e', hs) <- cfg e
  pure (CL.MkExprRef e', hs)
cfg (CL.MkExprUnref e) = do
  (e', hs) <- cfg e
  pure (CL.MkExprUnref e', hs)
cfg (CL.MkExprCast e t) = do
  (e', hs) <- cfg e
  pure (CL.MkExprCast e' t, hs)
cfg (CL.MkExprSizeOf t) = pure (CL.MkExprSizeOf t, [])

cfgStatement :: MonadIO m => CL.Statement -> m [CL.Statement]
cfgStatement (CL.MkStmtExpr e) = do
  (e', hs) <- cfg e
  pure $ hs <> [CL.MkStmtExpr e']
cfgStatement (CL.MkStmtIf c t e) = do
  (c', hs) <- cfg c
  t' <- concat <$> mapM cfgStatement t
  e' <- concat <$> mapM cfgStatement e
  pure $ hs <> [CL.MkStmtIf c' t' e']
cfgStatement (CL.MkStmtWhile c b) = do
  (c', hs) <- cfg c
  b' <- concat <$> mapM cfgStatement b
  pure $ hs <> [CL.MkStmtWhile c' b']
cfgStatement (CL.MkStmtFor i c u b) = do
  (i', hs) <- cfg i
  (c', hs') <- cfg c
  (u', hs'') <- cfg u
  b' <- concat <$> mapM cfgStatement b
  pure $ hs <> hs' <> hs'' <> [CL.MkStmtFor i' c' u' b']
cfgStatement (CL.MkStmtReturn e) = do
  (e', hs) <- cfg e
  pure $ hs <> [CL.MkStmtReturn e']
cfgStatement CL.MkStmtBreak = pure [CL.MkStmtBreak]
cfgStatement CL.MkStmtContinue = pure [CL.MkStmtContinue]
cfgStatement (CL.MkStmtBlock es) = do
  sts <- concat <$> mapM cfgStatement es
  pure [CL.MkStmtBlock sts]
cfgStatement (CL.MkStmtDeclare n t) = pure [CL.MkStmtDeclare n t]
cfgStatement (CL.MkStmtAssign v e) = do
  (e', hs) <- cfg e
  pure $ hs <> [CL.MkStmtAssign v e']
cfgStatement (CL.MkStmtDeclareAssign n t e) = do
  (e', hs) <- cfg e
  pure $ hs <> [CL.MkStmtDeclareAssign n t e']

cfgUpdate :: MonadIO m => CL.Update -> m (CL.Update, [CL.Statement])
cfgUpdate (CL.MkUpdtVariable a t) = pure (CL.MkUpdtVariable a t, [])
cfgUpdate (CL.MkUpdtField u f) = do
  (u', hs) <- cfgUpdate u
  pure (CL.MkUpdtField u' f, hs)
cfgUpdate (CL.MkUpdtIndex u e) = do
  (u', hs) <- cfgUpdate u
  (e', hs') <- cfg e
  pure (CL.MkUpdtIndex u' e', hs <> hs')
cfgUpdate (CL.MkUpdtUnref u) = do
  (u', hs) <- cfgUpdate u
  pure (CL.MkUpdtUnref u', hs)

runCFGConversion :: MonadIO m => [CL.Toplevel] -> m [CL.Toplevel]
runCFGConversion (CL.MkTopFunction n args t ss : ts) = do
  ss' <- concat <$> mapM cfgStatement ss

  ts' <- runCFGConversion ts
  pure $ CL.MkTopFunction n args t ss' : ts'
runCFGConversion (CL.MkTopStruct n ss : ts) = do
  ts' <- runCFGConversion ts
  pure $ CL.MkTopStruct n ss : ts'
runCFGConversion (CL.MkTopUnion n ss : ts) = do
  ts' <- runCFGConversion ts
  pure $ CL.MkTopUnion n ss : ts'
runCFGConversion (CL.MkTopEnum n es : ts) = do
  ts' <- runCFGConversion ts
  pure $ CL.MkTopEnum n es : ts'
runCFGConversion (CL.MkTopExtern n args ret : ts) = do
  ts' <- runCFGConversion ts
  pure $ CL.MkTopExtern n args ret : ts'
runCFGConversion (CL.MkTopTypedef n args ret : ts) = do
  ts' <- runCFGConversion ts
  pure $ CL.MkTopTypedef n args ret : ts'
runCFGConversion (CL.MkTopForwardDecl n args ret : ts) = do
  ts' <- runCFGConversion ts
  pure $ CL.MkTopForwardDecl n args ret : ts'
runCFGConversion [] = pure []