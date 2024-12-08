{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.Bonzai.Backend.Closure.Typed where

import qualified Language.Bonzai.Syntax.TMLIR as TMLIR
import qualified GHC.IO as IO
import qualified Data.Map as Map
import qualified Language.Bonzai.Backend.Closure.Free as M
import Control.Monad.Result (compilerError)

{-# NOINLINE natives #-}
natives :: IORef (Map Text (Int, TMLIR.Type))
natives = IO.unsafePerformIO $ newIORef defaultNatives

{-# NOINLINE lambdaCount #-}
lambdaCount :: IORef Int
lambdaCount = IO.unsafePerformIO $ newIORef 0

freshLambda :: MonadIO m => Text -> m Text
freshLambda prefix = do
  modifyIORef' lambdaCount (+1)
  n <- readIORef lambdaCount
  pure $ "@@lambda_" <> prefix <> show n

{-# NOINLINE events #-}
events :: IORef (Map Text Int)
events = IO.unsafePerformIO $ newIORef mempty

defaultNatives :: Map Text (Int, TMLIR.Type)
defaultNatives = Map.fromList [
    ("create_actor", (1, [MkTyAny] TMLIR.:->: TMLIR.MkTyMutable (TMLIR.MkTyId "actor_t")))
  , ("send_message", (2, [TMLIR.MkTyMutable (TMLIR.MkTyId "actor_t"), TMLIR.MkTyId "message_t"] TMLIR.:->: TMLIR.MkTyUnit))
  ]

{-# NOINLINE globals #-}
globals :: IORef (Map Text (Int, TMLIR.Type))
globals = IO.unsafePerformIO $ newIORef mempty

interfaces :: IORef [TMLIR.Expression]
interfaces = IO.unsafePerformIO $ newIORef []

closures :: IORef (Map Text TMLIR.Type)
closures = IO.unsafePerformIO $ newIORef mempty

convert :: MonadIO m => TMLIR.TMLIR "expression" -> m (TMLIR.TMLIR "expression")
convert (TMLIR.MkExprLiteral l) = pure (TMLIR.MkExprLiteral l)
convert (TMLIR.MkExprVariable v t) = pure (TMLIR.MkExprVariable v t)
convert (TMLIR.MkExprLet gens x _ (TMLIR.MkExprLambda args ret body)) = do
  ty <- convertType (map (.value) args TMLIR.:->: ret)

  b <- convertLambda (Map.singleton x ty) (TMLIR.MkExprLambda args ret body)
  
  pure $ TMLIR.MkExprLet gens x ty b
convert (TMLIR.MkExprLambda args ret body) = convertLambda mempty (TMLIR.MkExprLambda args ret body)
convert (TMLIR.MkExprApplication f args t) = do
  globals' <- readIORef globals
  natives' <- readIORef natives

  let reserved = globals' <> natives'

  args' <- mapM (\case
    TMLIR.MkExprVariable name ty@(TMLIR.MkTyFun argsTys ret) -> case Map.lookup name reserved of
      Just (arity, _) | arity >= 0 -> do
        let args' = take arity $ map (\n -> "arg" <> show n) [(0 :: Integer)..]

        convert $ TMLIR.MkExprLambda (zipWith TMLIR.MkAnnotation args' argsTys) ret (TMLIR.MkExprApplication (TMLIR.MkExprVariable name ty) (zipWith TMLIR.MkExprVariable args' argsTys) ty)
      _ -> pure $ TMLIR.MkExprVariable name ty
    e -> convert e) args

  case f of
    TMLIR.MkExprVariable name _ | Map.member name reserved -> do
      print (name, t)

      pure $ TMLIR.MkExprApplication f args' t

    _ | argsTys TMLIR.:->: ret <- t -> do
      name <- freshLambda "call"
      f' <- convert f

      callTy <- convertType (argsTys TMLIR.:->: ret)

      let callVar = TMLIR.MkExprVariable name callTy
      let function = TMLIR.MkExprField (TMLIR.MkExprUnref callVar) "fun"
      let env = TMLIR.MkExprField (TMLIR.MkExprUnref callVar) "env"

      let call = TMLIR.MkExprApplication function (env : args') ret

      pure $ TMLIR.MkExprUnpack name callTy f' call
    _ -> compilerError "unimplemented"
convert (TMLIR.MkExprBlock es t) = TMLIR.MkExprBlock <$> mapM convert es <*> pure t
convert (TMLIR.MkExprActor {}) = compilerError "Cannot convert actor"
convert (TMLIR.MkExprOn {}) = compilerError "Cannot convert on"
convert (TMLIR.MkExprSend {}) = compilerError "Cannot convert send"
convert (TMLIR.MkExprSpawn _) = compilerError "Cannot convert spawn"
convert (TMLIR.MkExprList es) = do
  es' <- mapM convert es
  pure $ TMLIR.MkExprList es'
convert (TMLIR.MkExprNative n t) = pure $ TMLIR.MkExprNative n t
convert (TMLIR.MkExprIndex e i) = do
  e' <- convert e
  i' <- convert i
  pure $ TMLIR.MkExprIndex e' i'
convert (TMLIR.MkExprUnpack n t e e') = do
  e'' <- convert e
  e''' <- convert e'
  pure $ TMLIR.MkExprUnpack n t e'' e'''
convert (TMLIR.MkExprWhile c e) = do
  c' <- convert c
  e' <- convert e
  pure $ TMLIR.MkExprWhile c' e'
convert (TMLIR.MkExprInterface qvs as) = pure $ TMLIR.MkExprInterface qvs as
convert (TMLIR.MkExprData qvs dcs) = pure $ TMLIR.MkExprData qvs dcs
convert (TMLIR.MkExprTernary c t e ty) = do
  c' <- convert c
  t' <- convert t
  e' <- convert e
  pure $ TMLIR.MkExprTernary c' t' e' ty
convert (TMLIR.MkExprUpdate u e) = do
  u' <- convertUpdate u
  e' <- convert e
  pure $ TMLIR.MkExprUpdate u' e'
convert (TMLIR.MkExprField e f) = do
  e' <- convert e
  pure $ TMLIR.MkExprField e' f
convert (TMLIR.MkExprLet gens x t e) = do
  e' <- convert e
  t' <- convertType t
  pure $ TMLIR.MkExprLet gens x t' e'
convert (TMLIR.MkExprStruct n anns) = do
  anns' <- mapM (\case
    (n', e) -> do
      e' <- convert e
      pure (n', e')
    ) anns
  pure $ TMLIR.MkExprStruct n anns'
convert (TMLIR.MkExprRef e t) = do
  e' <- convert e
  pure $ TMLIR.MkExprRef e' t
convert (TMLIR.MkExprUnref e) = do
  e' <- convert e
  pure $ TMLIR.MkExprUnref e'
convert (TMLIR.MkExprCast e t) = do
  e' <- convert e
  pure $ TMLIR.MkExprCast e' t
convert (TMLIR.MkExprSizeOf t) = pure $ TMLIR.MkExprSizeOf t

convertUpdate :: MonadIO m => TMLIR.TMLIR "update" -> m (TMLIR.TMLIR "update")
convertUpdate (TMLIR.MkUpdtVariable a t) = pure $ TMLIR.MkUpdtVariable a t
convertUpdate (TMLIR.MkUpdtField u f) = do
  u' <- convertUpdate u
  pure $ TMLIR.MkUpdtField u' f
convertUpdate (TMLIR.MkUpdtIndex u e) = do
  u' <- convertUpdate u
  e' <- convert e
  pure $ TMLIR.MkUpdtIndex u' e'
convertUpdate (TMLIR.MkUpdtUnref u) = do
  u' <- convertUpdate u
  pure $ TMLIR.MkUpdtUnref u'

convertType :: MonadIO m => TMLIR.Type -> m TMLIR.Type
convertType (TMLIR.MkTyActor _) = do
  pure $ [TMLIR.MkTyId "actor_t", TMLIR.MkTyId "message_t"] TMLIR.:->: TMLIR.MkTyUnit
convertType (TMLIR.MkTyFun a b) = do
  a' <- mapM convertType a
  b' <- convertType b

  lambdaCl <- freshLambda "closure"
  let lambdaClStruct = TMLIR.MkExprInterface (TMLIR.MkAnnotation lambdaCl []) [TMLIR.MkAnnotation "env" MkTyAny, TMLIR.MkAnnotation "fun" ((MkTyAny : a') TMLIR.:->: b')]

  modifyIORef' interfaces ([lambdaClStruct] <>)

  pure $ TMLIR.MkTyApp (TMLIR.MkTyId "ref") [TMLIR.MkTyId lambdaCl]
convertType (TMLIR.MkTyApp a b) = do
  a' <- convertType a
  b' <- mapM convertType b
  pure $ TMLIR.MkTyApp a' b'
convertType (TMLIR.MkTyId a) = pure $ TMLIR.MkTyId a
convertType (TMLIR.MkTyVar a) = do
  a' <- readIORef a
  case a' of
    TMLIR.Link b -> convertType b
    _ -> pure $ TMLIR.MkTyVar a
convertType a = pure a

convertLambda :: MonadIO m => Map Text TMLIR.Type -> TMLIR.TMLIR "expression" -> m (TMLIR.TMLIR "expression")
convertLambda reserved (TMLIR.MkExprLambda args ret_ body) = do
  let freeVars = M.tFree body
  nativesFuns <- readIORef natives
  globals' <- readIORef globals

  ret <- convertType ret_

  let reserved' = globals' <> nativesFuns

  let finalNativesFuns = Map.map snd reserved'
  args' <- mapM (\(TMLIR.MkAnnotation n t) -> TMLIR.MkAnnotation n <$> funToAny t) args
  let args'' = Map.fromList $ map TMLIR.unannotate args'

  let argsTys = Map.elems args''
  let finalNativesFuns' = finalNativesFuns Map.\\ args''

  let env = freeVars Map.\\ (finalNativesFuns' <> args'' <> reserved)
  envAsList <- mapM (\(x, t) -> (x,) <$> convertType t) (Map.toList env)

  envName <- freshLambda "env"
  let envStruct = TMLIR.MkExprInterface (TMLIR.MkAnnotation envName []) (map (uncurry TMLIR.MkAnnotation) envAsList)

  let prefixBody = map (\(n, t) -> do
          TMLIR.MkExprLet mempty n t (TMLIR.MkExprField (TMLIR.MkExprUnref (TMLIR.MkExprVariable "env" MkTyAny)) n)
        ) envAsList

  body' <- convert body

  let finalBody = case body' of
          TMLIR.MkExprBlock es t -> TMLIR.MkExprBlock (prefixBody <> es) t
          e -> TMLIR.MkExprBlock (prefixBody <> [e]) ret

  let envStructE = TMLIR.MkExprStruct envName (map (\(n, t) -> do
          (Just n, TMLIR.MkExprVariable n t)
        ) envAsList)

  lambdaName <- freshLambda "lambda"
  let funTy = (TMLIR.MkTyMutable (TMLIR.MkTyId envName) : argsTys) TMLIR.:->: ret
  let lambdaStruct = TMLIR.MkExprInterface (TMLIR.MkAnnotation lambdaName []) [TMLIR.MkAnnotation "env" (TMLIR.MkTyMutable (TMLIR.MkTyId envName)), TMLIR.MkAnnotation "fun" funTy]

  let lambdaExprE = TMLIR.MkExprStruct lambdaName [
          (Just "env", TMLIR.MkExprRef envStructE (TMLIR.MkTyId envName)),
          (Just "fun", TMLIR.MkExprCast (TMLIR.MkExprLambda (TMLIR.MkAnnotation "env" (TMLIR.MkTyMutable (TMLIR.MkTyId envName)) : args) ret finalBody) funTy)
        ]

  modifyIORef' interfaces ([envStruct, lambdaStruct] <>)

  pure (TMLIR.MkExprRef lambdaExprE (TMLIR.MkTyId lambdaName))
convertLambda _ e = convert e

pattern MkTyClosure :: TMLIR.Type -> TMLIR.Type -> TMLIR.Type
pattern MkTyClosure env fun = TMLIR.MkTyApp (TMLIR.MkTyId "Closure") [env, fun]

pattern MkTyAny :: TMLIR.Type
pattern MkTyAny = TMLIR.MkTyId "Any"

getArity :: TMLIR.Expression -> Int
getArity (TMLIR.MkExprLambda args _ _) = length args
getArity _ = -1

analyzeProgram :: MonadIO m => TMLIR.Expression -> m ()
analyzeProgram (TMLIR.MkExprLet _ x t e) = do
  modifyIORef' globals (Map.insert x (getArity e, t))
analyzeProgram (TMLIR.MkExprData name dcs) = do
  forM_ dcs $ \case
    TMLIR.MkDataConstructor n ts -> do
      modifyIORef' globals (Map.insert n (length ts, ts TMLIR.:->: TMLIR.MkTyId name.name))
    TMLIR.MkDataVariable n -> do
      modifyIORef' globals (Map.insert n (0, TMLIR.MkTyId name.name))
analyzeProgram (TMLIR.MkExprNative ann ty) = do
  let arity = case ty of
        args TMLIR.:->: _ -> length args
        _ -> (-1)
  modifyIORef' natives (Map.insert ann.name (arity, ty))
analyzeProgram _ = pure ()


funToAny :: MonadIO m => TMLIR.Type -> m TMLIR.Type
funToAny (TMLIR.MkTyActor _) = pure MkTyAny
funToAny (_ TMLIR.:->: _) = pure MkTyAny
funToAny (TMLIR.MkTyApp a b) = do
  a' <- funToAny a
  b' <- mapM funToAny b
  pure $ TMLIR.MkTyApp a' b'
funToAny (TMLIR.MkTyId a) = pure $ TMLIR.MkTyId a
funToAny (TMLIR.MkTyVar a) = do
  a' <- readIORef a
  case a' of
    TMLIR.Link b -> funToAny b
    _ -> pure $ TMLIR.MkTyVar a
funToAny a = pure a

convertToplevel :: MonadIO m => TMLIR.Expression -> m TMLIR.Expression
convertToplevel (TMLIR.MkExprLet gens x t e@(TMLIR.MkExprLambda args ret body)) = do
  args' <- mapM (\(TMLIR.MkAnnotation n t') -> TMLIR.MkAnnotation n <$> funToAny t') args
  modifyIORef' globals (Map.insert x (getArity e, t))
  body' <- convert body

  ret' <- funToAny ret

  pure $ TMLIR.MkExprLet gens x (map (.value) args' TMLIR.:->: ret') $ TMLIR.MkExprLambda args' ret' body'
convertToplevel e = convert e

runTypedClosureConversion :: MonadIO m => [TMLIR.Expression] -> m [TMLIR.Expression]
runTypedClosureConversion e = do
  mapM_ analyzeProgram e
  xs <- mapM convertToplevel e

  is <- readIORef interfaces

  pure $ is <> xs