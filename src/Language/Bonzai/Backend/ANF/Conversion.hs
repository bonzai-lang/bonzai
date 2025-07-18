module Language.Bonzai.Backend.ANF.Conversion where

import qualified Language.Bonzai.Syntax.MLIR as MLIR
import qualified GHC.IO as IO
import Language.Bonzai.Backend.Closure.Conversion (getVariable, isVariable)

-- | ANF CONVERSION
-- |
-- | This pass converts the MLIR to ANF by converting all expressions to a series
-- | of let expressions. For instance, unpack expressions are converted to let
-- | expressions, and so on.
-- |
-- | The conversion is done by traversing the AST and converting each expression
-- | to a series of let expressions. The conversion is done in a way that the
-- | resulting AST is in A-Normal Form.
-- |
-- | For example, the following expression:
-- |
-- | let x = 1 in x + 2
-- |
-- | will be converted to the following expression:
-- |
-- | let x = 1 
-- | let anf0 = x + 2
-- | anf0
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
  => MLIR.MLIR "expression" 
  -> m (MLIR.MLIR "expression", [(Text, MLIR.MLIR "expression")])
convert (MLIR.MkExprVariable a) = pure (MLIR.MkExprVariable a, [])
convert (MLIR.MkExprApplication f args) | isVariable f, name <- getVariable f = do
  (args', stmts) <- mapAndUnzipM convert args
  
  pure (MLIR.MkExprApplication (MLIR.MkExprVariable name) args', concat stmts)
convert (MLIR.MkExprApplication f args) = do
  (f', stmts1) <- convert f
  (args', stmts2) <- mapAndUnzipM convert args
  
  name <- freshSymbol "anf"

  let funCall = MLIR.MkExprApplication f' args'
      funVar  = MLIR.MkExprVariable name

  pure (funVar, stmts1 <> concat stmts2 <> [(name, funCall)])
convert (MLIR.MkExprLambda args body) = do
  (body', stmts) <- convert body
  
  let newBody = case body' of
        MLIR.MkExprBlock es -> MLIR.MkExprBlock $ map createLet stmts <> es
        _ -> MLIR.MkExprBlock $ map createLet stmts <> [body']

  pure (MLIR.MkExprLambda args newBody, [])
convert (MLIR.MkExprTernary c t e) = do
  (c', stmts1) <- convert c
  (t', stmts2) <- convert t
  (e', stmts3) <- convert e
  
  let bl2 = createBlock [(t', stmts2)]
  let bl3 = createBlock [(e', stmts3)]

  pure (MLIR.MkExprTernary c' (MLIR.MkExprBlock bl2) (MLIR.MkExprBlock bl3), stmts1)
convert (MLIR.MkExprUpdate u e) = do
  (u', stmts1) <- convertUpdate u
  (e', stmts2) <- convert e
  
  pure (MLIR.MkExprUpdate u' e', stmts1 <> stmts2)
convert (MLIR.MkExprLet a e) = do
  (e', stmts) <- convert e
  
  pure (MLIR.MkExprLet a e', stmts)
convert (MLIR.MkExprMut e) = do
  (e', stmts) <- convert e
  
  pure (MLIR.MkExprMut e', stmts)
convert (MLIR.MkExprBlock es) = do
  es' <- mapM convert es
  let exprs = createBlock es'
  
  pure (MLIR.MkExprBlock exprs, [])
convert (MLIR.MkExprList es) = do
  (es', stmts) <- mapAndUnzipM convert es
  
  name <- freshSymbol "anf"
  let list = MLIR.MkExprList es'
      letVar = MLIR.MkExprVariable name

  pure (letVar, concat stmts <> [(name, list)])
convert (MLIR.MkExprNative n ty) = pure (MLIR.MkExprNative n ty, [])
convert (MLIR.MkExprIndex e i) = do
  (e', stmts1) <- convert e
  (i', stmts2) <- convert i
  
  pure (MLIR.MkExprIndex e' i', stmts1 <> stmts2)
convert (MLIR.MkExprLiteral l) = pure (MLIR.MkExprLiteral l, [])
convert (MLIR.MkExprUnpack n e e') = do
  (e'', stmts1) <- convert e
  (e''', stmts2) <- convert e'

  name <- freshSymbol "anf"
  
  pure (MLIR.MkExprVariable name, stmts1 <> [(n, e'')] <> stmts2 <> [(name, e''')])
convert (MLIR.MkExprLoc p e) = do
  (e', stmts) <- convert e
  
  pure (MLIR.MkExprLoc p e', stmts)
convert (MLIR.MkExprWhile c e) = do
  (c', stmts1) <- convert c
  (e', stmts2) <- convert e

  let eBl = createBlock [(e', stmts2)]
  let cBl = createBlock [(c', stmts1)]
  
  pure (MLIR.MkExprWhile (MLIR.MkExprBlock cBl) (MLIR.MkExprBlock eBl), [])
convert MLIR.MkExprSpecial = pure (MLIR.MkExprSpecial, [])
convert (MLIR.MkExprBinary op e1 e2) = do
  (e1', stmts1) <- convert e1
  (e2', stmts2) <- convert e2
  
  pure (MLIR.MkExprBinary op e1' e2', stmts1 <> stmts2)
convert (MLIR.MkExprRecordAccess e f) = do
  (e', stmts) <- convert e
  
  pure (MLIR.MkExprRecordAccess e' f, stmts)
convert (MLIR.MkExprSingleIf c e) = do
  (c', stmts1) <- convert c
  (e', stmts2) <- convert e
  
  let bl2 = createBlock [(e', stmts2)]
  let bl1 = createBlock [(c', stmts1)]
  
  pure (MLIR.MkExprSingleIf (MLIR.MkExprBlock bl1) (MLIR.MkExprBlock bl2), [])
convert (MLIR.MkExprReturn e) = do
  (e', stmts) <- convert e
  
  pure (MLIR.MkExprReturn e', stmts)
convert (MLIR.MkExprRecord m) = do
  (stmts, m') <- mapM swap <$> mapM convert m
  
  pure (MLIR.MkExprRecord m', stmts)
convert MLIR.MkExprBreak = pure (MLIR.MkExprBreak, [])
convert MLIR.MkExprContinue = pure (MLIR.MkExprContinue, [])
convert (MLIR.MkExprSpawn e) = do
  (e', stmts) <- convert e

  name <- freshSymbol "anf"
  let spawnVar = MLIR.MkExprVariable name
      spawnExpr = MLIR.MkExprSpawn e'

  pure (spawnVar, stmts <> [(name, spawnExpr)])

convertUpdate :: MonadIO m => MLIR.MLIR "update" -> m (MLIR.MLIR "update", [(Text, MLIR.MLIR "expression")])
convertUpdate (MLIR.MkUpdtVariable a) = pure (MLIR.MkUpdtVariable a, [])
convertUpdate (MLIR.MkUpdtField u f) = do
  (u', stmts) <- convertUpdate u
  
  pure (MLIR.MkUpdtField u' f, stmts)
convertUpdate (MLIR.MkUpdtIndex u e) = do
  (u', stmts1) <- convertUpdate u
  (e', stmts2) <- convert e
  
  pure (MLIR.MkUpdtIndex u' e', stmts1 <> stmts2)

createBlock :: [(MLIR.Expression, [(Text, MLIR.Expression)])] -> [MLIR.Expression]
createBlock ((e, stmts) : xs) = map createLet stmts <> [e] <> createBlock xs
createBlock [] = []

createLet :: (Text, MLIR.Expression) -> MLIR.Expression
createLet (n, e) = MLIR.MkExprLet n e

runANFConversion :: MonadIO m => [MLIR.MLIR "expression"] -> m [MLIR.MLIR "expression"]
runANFConversion xs = createBlock <$> mapM convert xs