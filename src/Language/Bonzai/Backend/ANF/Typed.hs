{-# LANGUAGE LambdaCase #-}
module Language.Bonzai.Backend.ANF.Typed where

import qualified Language.Bonzai.Syntax.TMLIR as MLIR
import qualified GHC.IO as IO

{-# NOINLINE symbolCounter #-}
symbolCounter :: IORef Int
symbolCounter = IO.unsafePerformIO $ newIORef 0

freshSymbol :: MonadIO m => Text -> m Text
freshSymbol prefix = do
  i <- readIORef symbolCounter
  modifyIORef' symbolCounter (+1)
  
  pure $ prefix <> show i

convert 
  :: MonadIO m 
  => MLIR.TMLIR "expression" 
  -> m (MLIR.TMLIR "expression", [MLIR.TMLIR "expression"])
convert (MLIR.MkExprVariable a t) = pure (MLIR.MkExprVariable a t, [])
convert (MLIR.MkExprApplication f args t) = do
  (f', stmts1) <- convert f
  (args', stmts2) <- mapAndUnzipM convert args
  
  pure (MLIR.MkExprApplication f' args' t, stmts1 <> concat stmts2)
convert (MLIR.MkExprLambda args ret body) = do
  (body', stmts) <- convert body
  
  let newBody = case body' of
        MLIR.MkExprBlock es t -> MLIR.MkExprBlock (stmts <> es) t
        _ -> MLIR.MkExprBlock (stmts <> [body']) ret

  pure (MLIR.MkExprLambda args ret newBody, [])
convert (MLIR.MkExprTernary c t e ty) = do
  (c', stmts1) <- convert c
  (t', stmts2) <- convert t
  (e', stmts3) <- convert e
  
  pure (MLIR.MkExprTernary c' t' e' ty, stmts1 <> stmts2 <> stmts3)
convert (MLIR.MkExprUpdate u e) = do
  (u', stmts1) <- convertUpdate u
  (e', stmts2) <- convert e
  
  pure (MLIR.MkExprUpdate u' e', stmts1 <> stmts2)
convert (MLIR.MkExprLet g a t e) = do
  (e', stmts) <- convert e
  
  pure (MLIR.MkExprLet g a t e', stmts)
convert (MLIR.MkExprBlock es t) = do
  es' <- mapM convert es
  let exprs = createBlock es'
  
  pure (MLIR.MkExprBlock exprs t, [])
convert (MLIR.MkExprActor i es) = do
  (es', stmts) <- mapAndUnzipM convert es
  
  pure (MLIR.MkExprActor i es', concat stmts)
convert (MLIR.MkExprOn ev args e) = do
  (e', stmts) <- convert e
  
  pure (MLIR.MkExprOn ev args e', stmts)
convert (MLIR.MkExprSend e ev es t) = do
  (e', stmts1) <- convert e
  (es', stmts2) <- mapAndUnzipM convert es
  
  pure (MLIR.MkExprSend e' ev es' t, stmts1 <> concat stmts2)
convert (MLIR.MkExprSpawn e) = do
  (e', stmts) <- convert e
  
  pure (MLIR.MkExprSpawn e', stmts)
convert (MLIR.MkExprList es) = do
  (es', stmts) <- mapAndUnzipM convert es
  
  pure (MLIR.MkExprList es', concat stmts)
convert (MLIR.MkExprNative n ty) = pure (MLIR.MkExprNative n ty, [])
convert (MLIR.MkExprIndex e i) = do
  (e', stmts1) <- convert e
  (i', stmts2) <- convert i
  
  pure (MLIR.MkExprIndex e' i', stmts1 <> stmts2)
convert (MLIR.MkExprLiteral l) = pure (MLIR.MkExprLiteral l, [])
convert (MLIR.MkExprUnpack n t e e') = do
  (e'', stmts1) <- convert e
  (e''', stmts2) <- convert e'

  -- name <- freshSymbol "anf"
  
  pure (e''', stmts1 <> [MLIR.MkExprLet mempty n t e''] <> stmts2)
convert (MLIR.MkExprWhile c e) = do
  (c', stmts1) <- convert c
  (e', stmts2) <- convert e
  
  pure (MLIR.MkExprWhile c' e', stmts1 <> stmts2)
convert (MLIR.MkExprField e f) = do
  (e', stmts) <- convert e
  
  pure (MLIR.MkExprField e' f, stmts)
convert (MLIR.MkExprData header anns) = pure (MLIR.MkExprData header anns, [])
convert (MLIR.MkExprInterface header anns) = pure (MLIR.MkExprInterface header anns, [])
convert (MLIR.MkExprStruct n anns) = do
  (stmtss, anns') <- mapAndUnzipM (\case
    (n', e) -> do
      (e', stmts) <- convert e
      pure (stmts, (n', e'))
    ) anns

  pure (MLIR.MkExprStruct n anns', concat stmtss)
convert (MLIR.MkExprRef e t) = do
  (e', stmts) <- convert e

  name <- freshSymbol "ref"
  exprName <- freshSymbol "expr"

  let refFunTy = [MLIR.MkTyInt] MLIR.:->: MLIR.MkTyMutable t

  let firstDecl = MLIR.MkExprLet mempty exprName t e'
  let seconDecl = MLIR.MkExprLet mempty name (MLIR.MkTyMutable t) (MLIR.MkExprApplication (MLIR.MkExprVariable "malloc" refFunTy) [MLIR.MkExprSizeOf t] (MLIR.MkTyMutable t))
  let thirdDecl = MLIR.MkExprUpdate (MLIR.MkUpdtVariable name (MLIR.MkTyMutable t)) (MLIR.MkExprVariable exprName t)

  pure (MLIR.MkExprVariable name (MLIR.MkTyMutable t), stmts <> [firstDecl, seconDecl, thirdDecl])
convert (MLIR.MkExprUnref e) = do
  (e', stmts) <- convert e
  
  pure (MLIR.MkExprUnref e', stmts)
convert (MLIR.MkExprCast e t) = do
  (e', stmts) <- convert e
  
  pure (MLIR.MkExprCast e' t, stmts)
convert (MLIR.MkExprSizeOf t) = pure (MLIR.MkExprSizeOf t, [])

convertUpdate :: MonadIO m => MLIR.TMLIR "update" -> m (MLIR.TMLIR "update", [MLIR.Expression])
convertUpdate (MLIR.MkUpdtVariable a t) = pure (MLIR.MkUpdtVariable a t, [])
convertUpdate (MLIR.MkUpdtField u f) = do
  (u', stmts) <- convertUpdate u
  
  pure (MLIR.MkUpdtField u' f, stmts)
convertUpdate (MLIR.MkUpdtIndex u e) = do
  (u', stmts1) <- convertUpdate u
  (e', stmts2) <- convert e
  
  pure (MLIR.MkUpdtIndex u' e', stmts1 <> stmts2)
convertUpdate (MLIR.MkUpdtUnref u) = do
  (u', stmts) <- convertUpdate u
  
  pure (MLIR.MkUpdtUnref u', stmts)

createBlock :: [(MLIR.Expression, [MLIR.Expression])] -> [MLIR.Expression]
createBlock ((e, stmts) : xs) = stmts <> [e] <> createBlock xs
createBlock [] = []


runANFConversion :: MonadIO m => [MLIR.TMLIR "expression"] -> m [MLIR.TMLIR "expression"]
runANFConversion xs = createBlock <$> mapM convert xs