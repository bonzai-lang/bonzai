module Language.Bonzai.Backend.TypeConversion.Actor where

import qualified Language.Bonzai.Syntax.TMLIR as MLIR
import Control.Monad.Result (compilerError)
import Language.Bonzai.Backend.TypeConversion.Conversion (panicTy, strEqualsTo)
import qualified GHC.IO as IO
import qualified Language.Bonzai.Backend.Closure.Typed as MLIR

{-# NOINLINE counter #-}
counter :: IORef Int
counter = IO.unsafePerformIO $ newIORef 0

freshVar :: MonadIO m => Text -> m Text
freshVar prefix = do
  i <- readIORef counter
  modifyIORef' counter (+1)

  pure $ prefix <> show i

decomposeHeader :: MonadIO m => MLIR.Type -> m (Text, [MLIR.Type])
decomposeHeader (MLIR.MkTyApp (MLIR.MkTyId n) ts) = pure (n, ts)
decomposeHeader (MLIR.MkTyId n) = pure (n, [])
decomposeHeader t = compilerError $ "Cannot decompose header " <> show t

convert :: MonadIO m => MLIR.Expression -> m (MLIR.Expression, [MLIR.Expression])
convert (MLIR.MkExprSpawn e) = do
  (e', stmts) <- convert e

  let actorTy = [] MLIR.:->: MLIR.MkTyAny
  let funTy = [MLIR.MkTyAny] MLIR.:->: MLIR.MkTyMutable (MLIR.MkTyId "actor_t")

  pure (MLIR.MkExprApplication (MLIR.MkExprVariable "create_actor" funTy) [MLIR.MkExprApplication e' [] actorTy] funTy, stmts)
convert (MLIR.MkExprActor i es) = do
  (_, actorArgs) <- decomposeHeader i

  (events, es') <- mapAndUnzipM (convertOn actorArgs) es

  let (es'', interfaces) = unzip es'

  let conds = createConditions events

  let body = es'' <> [MLIR.MkExprLambda [MLIR.MkAnnotation "actor" (MLIR.MkTyId "actor_t"), MLIR.MkAnnotation "message" (MLIR.MkTyId "message_t")] MLIR.MkTyUnit conds]

  let funTy = [MLIR.MkTyId "actor_t", MLIR.MkTyId "message_t"] MLIR.:->: MLIR.MkTyUnit

  pure (MLIR.MkExprLambda [] funTy (MLIR.MkExprBlock body (MLIR.MkTyId "actor_t")), concat interfaces)
  where
    convertOn :: MonadIO m => [MLIR.Type] -> MLIR.Expression -> m ((Text, MLIR.Type), (MLIR.Expression, [MLIR.Expression]))
    convertOn tys (MLIR.MkExprOn ev args e) = do
      qvars <- mapM getQVar tys
      let funTy = [MLIR.MkTyAny] MLIR.:->: MLIR.MkTyId "actor_t"

      let interface = MLIR.MkExprInterface (MLIR.MkAnnotation ev qvars) args

      let val = MLIR.MkExprVariable "value" MLIR.MkTyAny
      let arg = MLIR.MkExprVariable "args" (MLIR.MkTyMutable (MLIR.MkTyId ev))
      let args' = [MLIR.MkExprLet mempty "args" (MLIR.MkTyMutable (MLIR.MkTyId ev)) val] <> map (\(MLIR.MkAnnotation n t) -> MLIR.MkExprLet mempty n t (MLIR.MkExprField (MLIR.MkExprUnref arg) n)) args

      let resFunTy = [MLIR.MkTyAny] MLIR.:->: MLIR.MkTyUnit

      pure ((ev, funTy), (MLIR.MkExprLet mempty ev resFunTy (MLIR.MkExprLambda [MLIR.MkAnnotation "value" MLIR.MkTyAny ] MLIR.MkTyUnit (MLIR.MkExprBlock (args' <> [e]) MLIR.MkTyUnit)), [interface]))
    convertOn _  e = compilerError $ "Cannot convert " <> show e

    getQVar :: MonadIO m => MLIR.Type -> m Text
    getQVar (MLIR.MkTyQuantified t) = pure t
    getQVar (MLIR.MkTyId t) = pure t
    getQVar (MLIR.MkTyVar tvr) = do
      val <- readIORef tvr

      case val of
        MLIR.Unbound t _ -> pure t
        MLIR.Link t -> getQVar t
    getQVar t = compilerError $ "Cannot get quantified variable from " <> show t

    message :: MLIR.Expression
    message = MLIR.MkExprVariable "message" (MLIR.MkTyId "message_t")

    createConditions :: [(Text, MLIR.Type)] -> MLIR.Expression
    createConditions [] = MLIR.MkExprApplication (MLIR.MkExprVariable "panic" panicTy) [MLIR.MkExprLiteral (MLIR.MkLitString "No conditions provided")] MLIR.MkTyUnit
    createConditions ((x, ty) : xs) = MLIR.MkExprTernary
      (strEqualsTo (MLIR.MkExprField message "name") x)
      (MLIR.MkExprApplication (MLIR.MkExprVariable x ty) [MLIR.MkExprField message "data"] ty)
      (createConditions xs)
      MLIR.MkTyUnit
convert (MLIR.MkExprLambda args ret e) = do
  (e', stmts) <- convert e

  pure (MLIR.MkExprLambda args ret e', stmts)
convert (MLIR.MkExprLet g n t e) = do
  (e', stmts) <- convert e

  pure (MLIR.MkExprLet g n t e', stmts)
convert (MLIR.MkExprBlock es t) = do
  (es', stmtss) <- mapAndUnzipM convert es

  pure (MLIR.MkExprBlock es' t, concat stmtss)
convert e = pure (e, [])

runActorConversion
  :: MonadIO m => [MLIR.Expression] -> m [MLIR.Expression]
runActorConversion (MLIR.MkExprLet g n t e : rest) = do
  (e', stmts) <- convert e
  rest' <- runActorConversion rest

  pure $ stmts <> [MLIR.MkExprLet g n t e'] <> rest'
runActorConversion (e : rest) = do
  (e', stmts) <- convert e
  rest' <- runActorConversion rest

  pure $ stmts <> (e' : rest')
runActorConversion [] = pure []