{-# LANGUAGE LambdaCase #-}
module Language.Bonzai.Frontend.Typechecking.Checker where

import qualified Language.Bonzai.Syntax.HLIR as HLIR
import qualified Language.Bonzai.Frontend.Typechecking.Monad as M
import Control.Monad.Result
import qualified Data.Map as Map
import qualified Language.Bonzai.Frontend.Typechecking.Unification as U
import qualified Data.List as List
import qualified Data.Foldable as Foldable

typecheck :: M.MonadChecker m => HLIR.HLIR "expression" -> m (HLIR.TLIR "expression", HLIR.Type)
typecheck (HLIR.MkExprVariable ann) = do
  st <- readIORef M.checkerState
  case Map.lookup ann.name st.variables of
    Just s -> do
      ty <- M.instantiate s

      case ty of 
        HLIR.MkTyLive _ -> 
          pure (
            HLIR.MkExprUnwrapLive (HLIR.MkExprVariable ann { HLIR.value = Identity ty }),
            ty
          )
        _ -> 
          pure (
            HLIR.MkExprVariable ann { HLIR.value = Identity ty }, 
            ty
          )
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

  let finalExpr = case ty of
          HLIR.MkTyLive ret -> HLIR.MkExprLambda [] (Identity ret) expr'
          _ | containsLive expr' -> HLIR.MkExprLambda [] (Identity exprTy) expr'
          _ -> expr'

  pure (HLIR.MkExprLet ann { HLIR.value = Identity ty } finalExpr, HLIR.MkTyUnit)
typecheck (HLIR.MkExprMut ann expr) = do
  ty <- M.fresh
  let scheme = HLIR.Forall [] (HLIR.MkTyMutable ty)

  (expr', exprTy) <- M.with
    M.checkerState
    (\st -> st { M.variables = Map.insert ann.name scheme st.variables })
    $ typecheck expr

  ty `U.unifiesWith` exprTy

  modifyIORef' M.checkerState $ \st -> st { M.variables = Map.insert ann.name scheme st.variables }


  let finalExpr = case ty of
          HLIR.MkTyLive ret -> HLIR.MkExprLambda [] (Identity ret) expr'
          _ -> expr'

  pure (HLIR.MkExprMut ann { HLIR.value = Identity (HLIR.MkTyMutable ty) } finalExpr, exprTy)
typecheck (HLIR.MkExprBlock es) = do
  (es', tys) <- unzip <$> traverse typecheck es

  retTy <- maybe M.fresh pure (viaNonEmpty last tys)

  pure (HLIR.MkExprBlock es', retTy)
typecheck (HLIR.MkExprUpdate u e) = do
  (u', ty) <- typecheckUpdate u
  (e', exprTy) <- typecheck e

  case ty of
    HLIR.MkTyLive t -> do
      exprTy `U.unifiesWith` t
    
    _ -> ty `U.unifiesWith` HLIR.MkTyMutable exprTy

  let modifier = case ty of
        HLIR.MkTyLive _ -> HLIR.MkExprLambda [] (Identity exprTy) e'
        _ | containsLive e' -> HLIR.MkExprLambda [] (Identity exprTy) e'
        _ -> e'

  pure (HLIR.MkExprUpdate u' modifier, HLIR.MkTyUnit)
typecheck (HLIR.MkExprActor i es) = do
  checkSt <- readIORef M.checkerState

  methodsTys <- case Map.lookup i checkSt.interfaces of
    Just tys -> mapM M.instantiate tys
    Nothing -> throw (M.ActorNotFound i)

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
typecheck (HLIR.MkExprWhile c e) = do
  (c', ty) <- typecheck c
  (e', _) <- typecheck e

  ty `U.unifiesWith` HLIR.MkTyBool

  pure (HLIR.MkExprWhile c' e', HLIR.MkTyUnit)
typecheck (HLIR.MkExprIndex e i) = do
  (e', ty) <- typecheck e
  (i', idxTy) <- typecheck i

  tvar <- M.fresh

  ty `U.unifiesWith` HLIR.MkTyList tvar
  idxTy `U.unifiesWith` HLIR.MkTyInt

  pure (HLIR.MkExprIndex e' i', tvar)
typecheck (HLIR.MkExprMatch scrut cases) = do
  (scrut', scrutTy) <- typecheck scrut

  (cases', tys) <- unzip <$> traverse (\(pat, e) -> do
      (pat', patTy, env) <- typecheckPattern pat
      scrutTy `U.unifiesWith` patTy

      (e', eTy) <- M.with M.checkerState (\st -> st { M.variables = env <> st.variables }) $ typecheck e

      pure ((pat', e'), eTy)
    ) cases

  (exprTy, exprTys) <- case tys of
    [] -> throw M.EmptyMatch
    (x : xs'') -> return (x, xs'')

  -- Unify the return type with the type of the case expressions
  forM_ exprTys $ U.unifiesWith exprTy

  pure (HLIR.MkExprMatch scrut' cases', exprTy)

typecheck (HLIR.MkExprData ann constrs) = do
  let name = ann.name
      generics = ann.value
      header = if null generics then HLIR.MkTyId name else HLIR.MkTyApp (HLIR.MkTyId name) (HLIR.MkTyQuantified <$> generics)

  let schemes = map (\case
          HLIR.MkDataConstructor constrName args ->
            (constrName, HLIR.Forall generics (args HLIR.:->: header))

          HLIR.MkDataVariable varName ->
            (varName, HLIR.Forall generics header)
        ) constrs

  modifyIORef M.checkerState $ \st -> st { M.variables = Map.fromList schemes <> st.variables }

  pure (HLIR.MkExprData ann constrs, HLIR.MkTyId name)
typecheck (HLIR.MkExprLive ann e) = do
  M.enterLevel
  ty <- M.fresh
  let scheme = HLIR.Forall [] (HLIR.MkTyLive ty)

  (expr', exprTy) <- M.with
    M.checkerState
    (\st -> st { M.variables = Map.insert ann.name scheme st.variables })
    $ typecheck e

  ty `U.unifiesWith` exprTy
  M.exitLevel

  newScheme <- M.generalize (HLIR.MkTyLive ty)

  modifyIORef' M.checkerState $ \st -> st { M.variables = Map.insert ann.name newScheme st.variables }

  pure (HLIR.MkExprLet ann { HLIR.value = Identity (HLIR.MkTyLive ty) } (HLIR.MkExprWrapLive expr'), HLIR.MkTyUnit)
typecheck (HLIR.MkExprRequire _) = compilerError "typecheck: require should not appear in typechecking"
typecheck (HLIR.MkExprUnwrapLive _) = compilerError "typecheck: unwrap should not appear in typechecking"
typecheck (HLIR.MkExprWrapLive _) = compilerError "typecheck: wrap should not appear in typechecking"

typecheckPattern :: M.MonadChecker m => HLIR.HLIR "pattern" -> m (HLIR.TLIR "pattern", HLIR.Type, Map Text HLIR.Scheme)
typecheckPattern (HLIR.MkPatVariable name varTy) = do
  st <- readIORef M.checkerState

  case Map.lookup name st.variables of
    Just s -> do
      ty' <- M.instantiate s
      pure (HLIR.MkPatVariable name (Identity ty'), ty', Map.empty)
    Nothing -> do
      ty <- maybe M.fresh pure varTy
      let scheme = HLIR.Forall [] ty
      pure (HLIR.MkPatVariable name (Identity ty), ty, Map.singleton name scheme)
typecheckPattern (HLIR.MkPatLiteral l) = do
  let ty = case l of
        HLIR.MkLitChar _ -> HLIR.MkTyChar
        HLIR.MkLitInt _ -> HLIR.MkTyInt
        HLIR.MkLitFloat _ -> HLIR.MkTyFloat
        HLIR.MkLitString _ -> HLIR.MkTyString
  pure (HLIR.MkPatLiteral l, ty, Map.empty)
typecheckPattern (HLIR.MkPatConstructor name pats) = do
  st <- readIORef M.checkerState
  case Map.lookup name st.variables of
    Just sch -> do
      ty <- M.instantiate sch
      case ty of
        tys HLIR.:->: ret -> do
          (pats', env) <- unzip <$> zipWithM (\pat ty' -> do
              (pat', ty'', env) <- typecheckPattern pat
              ty' `U.unifiesWith` ty''
              pure (pat', env)
            ) pats tys

          pure (HLIR.MkPatConstructor name pats', ret, Map.unions env)
        _ -> throw (M.InvalidConstructor name)
    Nothing -> throw (M.InvalidConstructor name)
typecheckPattern HLIR.MkPatWildcard = do
  ty <- M.fresh
  pure (HLIR.MkPatWildcard, ty, Map.empty)
typecheckPattern (HLIR.MkPatSpecial {}) =
  throw (CompilerError "typecheckPattern: special patterns are not supported")
typecheckPattern (HLIR.MkPatLocated p loc) =
  HLIR.pushPosition loc *> typecheckPattern p <* HLIR.popPosition
typecheckPattern (HLIR.MkPatOr p1 p2) = do
  (p1', ty, env1) <- typecheckPattern p1
  (p2', ty', env2) <- typecheckPattern p2

  ty `U.unifiesWith` ty'

  instEnv1 <- traverse M.instantiate env1
  instEnv2 <- traverse M.instantiate env2

  -- Check if the environments are disjoint
  common <- intersectionWithM U.doesUnifyWith instEnv1 instEnv2

  if null common || (Map.size common /= Map.size env1 && Map.size common /= Map.size env2)
    then throw (M.InvalidPatternUnion (Map.keysSet env1) (Map.keysSet env2)) 
    else if and common
      then pure (HLIR.MkPatOr p1' p2', ty, Map.union env1 env2)
      else throw (M.InvalidPatternUnion (Map.keysSet env1) (Map.keysSet env2))
typecheckPattern (HLIR.MkPatCondition e p) = do
  (p', ty, env) <- typecheckPattern p
  (e', ty') <- M.with M.checkerState (\st -> st { M.variables = env <> st.variables }) $ typecheck e

  ty' `U.unifiesWith` HLIR.MkTyBool

  pure (HLIR.MkPatCondition e' p', ty, env)

intersectionWithM :: (Ord k, Monad m) => (a -> b -> m c) -> Map k a -> Map k b -> m (Map k c)
intersectionWithM f m1 m2 = do
  let common = Map.intersection m1 m2
  Foldable.foldlM (\m (k, v) -> do
      v' <- f v (m2 Map.! k)
      pure $ Map.insert k v' m
    ) Map.empty (Map.toList common)

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

containsLive :: HLIR.TLIR "expression" -> Bool
containsLive (HLIR.MkExprLive _ _) = True
containsLive (HLIR.MkExprLoc e _) = containsLive e
containsLive (HLIR.MkExprApplication f args) = containsLive f || any containsLive args
containsLive (HLIR.MkExprLambda _ _ body) = containsLive body
containsLive (HLIR.MkExprLet _ e) = containsLive e
containsLive (HLIR.MkExprBlock es) = any containsLive es
containsLive (HLIR.MkExprTernary c t e) = containsLive c || containsLive t || containsLive e
containsLive (HLIR.MkExprUpdate _ e) = containsLive e
containsLive (HLIR.MkExprActor _ es) = any containsLive es
containsLive (HLIR.MkExprOn _ _ e) = containsLive e
containsLive (HLIR.MkExprSend e _ es) = containsLive e || any containsLive es
containsLive (HLIR.MkExprSpawn e) = containsLive e
containsLive (HLIR.MkExprList es) = any containsLive es
containsLive (HLIR.MkExprNative _ _) = False
containsLive (HLIR.MkExprMut _ e) = containsLive e
containsLive (HLIR.MkExprInterface _ _) = False
containsLive (HLIR.MkExprWhile c e) = containsLive c || containsLive e
containsLive (HLIR.MkExprIndex e e') = containsLive e || containsLive e'
containsLive (HLIR.MkExprData _ _) = False
containsLive (HLIR.MkExprMatch e cs) = containsLive e || any (containsLive . snd) cs
containsLive (HLIR.MkExprRequire _) = False
containsLive (HLIR.MkExprUnwrapLive _) = True
containsLive (HLIR.MkExprWrapLive e) = containsLive e
containsLive _ = False