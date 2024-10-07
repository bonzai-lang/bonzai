module Language.Bonzai.Frontend.Typechecking.Checker where

import qualified Language.Bonzai.Syntax.HLIR as HLIR
import qualified Language.Bonzai.Frontend.Typechecking.Monad as M
import Control.Monad.Result
import qualified Data.Map as Map
import qualified Language.Bonzai.Frontend.Typechecking.Unification as U
import qualified Data.List as List

typecheck :: M.MonadChecker m => HLIR.HLIR "expression" -> m (HLIR.TLIR "expression", HLIR.Type)
typecheck (HLIR.MkExprVariable ann) = do
  st <- readIORef M.checkerState
  case Map.lookup ann.name st.variables of
    Just s -> do
      ty <- M.instantiate s
      pure (HLIR.MkExprVariable ann { HLIR.value = Identity ty }, ty)
    Nothing -> throw (M.VariableNotFound ann.name)
typecheck (HLIR.MkExprLoc e p) = do
  HLIR.pushPosition p
  (e', ty) <- typecheck e
  void HLIR.popPosition
  pure (HLIR.MkExprLoc e' p, ty)
typecheck (HLIR.MkExprNative ann ty) = do
  let ty' = List.foldl (flip replaceIdwithQuVar) ty ann.value
  let scheme = HLIR.Forall ann.value ty'

  modifyIORef' M.checkerState $ \st -> st { M.variables = Map.insert ann.name scheme st.variables }

  pure (HLIR.MkExprNative ann ty', ty')

  where replaceIdwithQuVar :: Text -> HLIR.Type -> HLIR.Type
        replaceIdwithQuVar i (HLIR.MkTyId i') | i == i'
          = HLIR.MkTyQuantified i
        replaceIdwithQuVar i (HLIR.MkTyApp t1 t2)
          = HLIR.MkTyApp (replaceIdwithQuVar i t1) (replaceIdwithQuVar i <$> t2)
        replaceIdwithQuVar _ t = t
typecheck (HLIR.MkExprApplication f args) = do
  (f', ty) <- typecheck f
  (args', tys) <- unzip <$> traverse typecheck args

  ret <- M.fresh
  ty `U.unifiesWith` (tys HLIR.:->: ret)

  pure (HLIR.MkExprApplication f' args', ret)
typecheck (HLIR.MkExprLambda args _ body) = do
  args' <- traverse (\(HLIR.MkAnnotation ann _) -> do
      ty' <- M.fresh
      pure (ann, ty')
    ) args

  ret <- M.fresh

  let schemes = map (second $ HLIR.Forall []) args'

  body' <- M.with
    M.checkerState
    (\st -> st { M.variables = Map.union (Map.fromList schemes) st.variables })
    $ do
      (body', ret') <- typecheck body
      ret' `U.unifiesWith` ret
      pure body'

  let wfArgs = map (uncurry HLIR.MkAnnotation . second Identity) args'

  pure (HLIR.MkExprLambda wfArgs (Identity ret) body', map snd args' HLIR.:->: ret)
typecheck (HLIR.MkExprLiteral l) = do
  let ty = case l of
        HLIR.MkLitChar _ -> HLIR.MkTyChar
        HLIR.MkLitInt _ -> HLIR.MkTyInt
        HLIR.MkLitFloat _ -> HLIR.MkTyFloat
        HLIR.MkLitString _ -> HLIR.MkTyString
  pure (HLIR.MkExprLiteral l, ty)
typecheck (HLIR.MkExprTernary c t e) = do
  (c', ty) <- typecheck c
  (t', thenTy) <- typecheck t
  (e', elseTy) <- typecheck e

  thenTy `U.unifiesWith` elseTy
  ty `U.unifiesWith` HLIR.MkTyBool

  pure (HLIR.MkExprTernary c' t' e', thenTy)
typecheck (HLIR.MkExprLet ann expr) = do
  M.enterLevel
  ty <- M.fresh
  let scheme = HLIR.Forall [] ty

  (expr', exprTy) <- M.with
    M.checkerState
    (\st -> st { M.variables = Map.insert ann.name scheme st.variables })
    $ typecheck expr

  ty `U.unifiesWith` exprTy
  M.exitLevel

  newScheme <- M.generalize ty

  modifyIORef' M.checkerState $ \st -> st { M.variables = Map.insert ann.name newScheme st.variables }

  pure (HLIR.MkExprLet ann { HLIR.value = Identity ty } expr', exprTy)
typecheck (HLIR.MkExprMut ann expr) = do
  ty <- M.fresh
  let scheme = HLIR.Forall [] (HLIR.MkTyMutable ty)

  (expr', exprTy) <- M.with
    M.checkerState
    (\st -> st { M.variables = Map.insert ann.name scheme st.variables })
    $ typecheck expr

  ty `U.unifiesWith` exprTy

  modifyIORef' M.checkerState $ \st -> st { M.variables = Map.insert ann.name scheme st.variables }

  pure (HLIR.MkExprMut ann { HLIR.value = Identity (HLIR.MkTyMutable ty) } expr', exprTy)
typecheck (HLIR.MkExprBlock es) = do
  (es', tys) <- unzip <$> traverse typecheck es

  retTy <- maybe M.fresh pure (viaNonEmpty last tys)

  pure (HLIR.MkExprBlock es', retTy)
typecheck (HLIR.MkExprUpdate u e) = do
  (u', ty) <- typecheckUpdate u
  (e', exprTy) <- typecheck e

  ty `U.unifiesWith` HLIR.MkTyMutable exprTy

  pure (HLIR.MkExprUpdate u' e', ty)
typecheck (HLIR.MkExprActor i es) = do
  checkSt <- readIORef M.checkerState

  methodsTys <- case Map.lookup i checkSt.interfaces of
    Just tys -> mapM M.instantiate tys
    Nothing -> throw (M.EventNotFound i)

  (es', tys) <- unzip <$> traverse (`typecheckEvent` methodsTys) es

  let names = map getName es

  forM_ (zip names tys) $ \(name, ty) -> do
    case Map.lookup name methodsTys of
      Just ty' -> ty `U.unifiesWith` ty'
      Nothing -> throw (M.EventNotFound name)

  pure (HLIR.MkExprActor i es', HLIR.MkTyId i)

  where
    typecheckEvent :: M.MonadChecker m => HLIR.HLIR "expression" -> Map Text HLIR.Type -> m (HLIR.TLIR "expression", HLIR.Type)
    typecheckEvent (HLIR.MkExprOn ev args body) m = do
      argsTys HLIR.:->:_ <- case Map.lookup ev m of
        Just s -> pure s
        Nothing -> throw (M.EventNotFound ev)

      args' <- traverse (\(HLIR.MkAnnotation ann ty, expectedTy) -> case ty of
          Just annotation -> pure (ann, annotation)
          Nothing -> pure (ann, expectedTy)
        ) (zip args argsTys)

      let schemes = map (second $ HLIR.Forall []) args'

      body' <- M.with
        M.checkerState
        (\st -> st { M.variables = Map.union (Map.fromList schemes) st.variables })
        $ do
          (body', ret') <- typecheck body
          ret' `U.unifiesWith` HLIR.MkTyId "unit"
          pure body'

      let wfArgs = map (uncurry HLIR.MkAnnotation . second Identity) args'

      pure (HLIR.MkExprOn ev wfArgs body', map snd args' HLIR.:->: HLIR.MkTyId "unit")
    typecheckEvent (HLIR.MkExprLoc e p) m = do
      HLIR.pushPosition p
      (e', ty) <- typecheckEvent e m
      void HLIR.popPosition
      pure (HLIR.MkExprLoc e' p, ty)
    typecheckEvent e _ = compilerError $ "typecheck: event block should only contain let bindings or events, received " <> toText e
typecheck (HLIR.MkExprOn {}) = compilerError "typecheck: event block should only contain let bindings or events"
typecheck (HLIR.MkExprSend e ev a) = do
  (e', ty) <- typecheck e
  (a', elTys) <- mapAndUnzipM typecheck a

  case ty of
    HLIR.MkTyId actorName -> do
      checkSt <- readIORef M.checkerState
      case Map.lookup actorName checkSt.interfaces of
        Just tys -> do
          methodsTys <- mapM M.instantiate tys

          case Map.lookup ev methodsTys of
            Just (argsTy HLIR.:->: _) -> do
              if length argsTy /= length elTys
                then throw (M.InvalidArgumentQuantity (length argsTy) (length elTys))
                else do
                  zipWithM_ U.unifiesWith elTys argsTy
            _ -> throw (M.EventNotFound ev)
        Nothing -> throw (M.EventNotFound actorName)
    _ -> throw (M.ExpectedAnActor ty)

  pure (HLIR.MkExprSend e' ev a', HLIR.MkTyId "unit")
typecheck (HLIR.MkExprSpawn e) = do
  (e', ty) <- typecheck e
  pure (HLIR.MkExprSpawn e', ty)
typecheck (HLIR.MkExprList es) = do
  (es', tys) <- unzip <$> traverse typecheck es

  ty <- maybe M.fresh pure (viaNonEmpty head tys)
  zipWithM_ U.unifiesWith tys (repeat ty)

  pure (HLIR.MkExprList es', HLIR.MkTyList ty)
typecheck (HLIR.MkExprInterface ann defs) = do
  let name = ann.name
      generics = ann.value

  let schemes = map (\(HLIR.MkAnnotation funName funTy) -> (funName, HLIR.Forall generics funTy)) defs

  modifyIORef M.checkerState $ \st -> st { M.interfaces = Map.insert name (Map.fromList schemes) st.interfaces }

  pure (HLIR.MkExprInterface ann defs, HLIR.MkTyId name)
typecheck (HLIR.MkExprRequire _) = compilerError "typecheck: require should not appear in typechecking"
typecheck (HLIR.MkExprModule _ _) = compilerError "typecheck: module should not appear in typechecking"

typecheckUpdate :: M.MonadChecker m => HLIR.HLIR "update" -> m (HLIR.TLIR "update", HLIR.Type)
typecheckUpdate (HLIR.MkUpdtVariable ann) = do
  st <- readIORef M.checkerState
  case Map.lookup ann.name st.variables of
    Just s -> do
      ty <- M.instantiate s
      pure (HLIR.MkUpdtVariable ann { HLIR.value = Identity ty }, ty)
    Nothing -> throw (M.VariableNotFound ann.name)
typecheckUpdate (HLIR.MkUpdtField _ _) = compilerError "typecheckUpdate: field updates are not supported"
typecheckUpdate (HLIR.MkUpdtIndex u e) = do
  elTy <- M.fresh
  (u', ty) <- typecheckUpdate u
  (e', idxTy) <- typecheck e

  idxTy `U.unifiesWith` HLIR.MkTyInt
  HLIR.MkTyList elTy `U.unifiesWith` ty

  pure (HLIR.MkUpdtIndex u' e', elTy)

runTypechecking :: (MonadIO m, MonadFail m) => [HLIR.HLIR "expression"] -> m (Either M.Error [HLIR.TLIR "expression"])
runTypechecking es = do
  let st =
        M.MkCheckerState
          Map.empty
          Map.empty
  res <- M.with M.checkerState (const st) $ runExceptT $ traverse typecheck es
  pure $ case res of
    Left err -> Left err
    Right es' -> Right $ map fst es'

getName :: HLIR.HLIR "expression" -> Text
getName (HLIR.MkExprLet ann _) = ann.name
getName (HLIR.MkExprOn n _ _) = n
getName (HLIR.MkExprMut ann _) = ann.name
getName (HLIR.MkExprLoc e _) = getName e
getName e = compilerError $ "typecheck: event block should only contain let bindings or events, received " <> toText e
