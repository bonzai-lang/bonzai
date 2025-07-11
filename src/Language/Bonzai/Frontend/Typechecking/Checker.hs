{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Language.Bonzai.Frontend.Typechecking.Checker where

import Control.Monad.Result
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Language.Bonzai.Frontend.Typechecking.Monad qualified as M
import Language.Bonzai.Frontend.Typechecking.Unification qualified as U
import Language.Bonzai.Syntax.HLIR qualified as HLIR

mapWithAcc :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapWithAcc _ acc [] = (acc, [])
mapWithAcc f acc (x : xs) =
  let (acc', x') = f acc x
      (acc'', xs') = mapWithAcc f acc' xs
   in (acc'', x' : xs')

mapMWithAcc :: (Monad m) => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
mapMWithAcc _ acc [] = pure (acc, [])
mapMWithAcc f acc (x : xs) = do
  (acc', x') <- f acc x
  (acc'', xs') <- mapMWithAcc f acc' xs
  pure (acc'', x' : xs')

-- | Typecheck an expression : misworded, should be infer the type of an expression
-- | as bi-directional typechecking is not supported.
synthesize :: (M.MonadChecker m) => HLIR.HLIR "expression" -> m (HLIR.TLIR "expression", HLIR.Type)
synthesize (HLIR.MkExprVariable ann) = do
  st <- readIORef M.checkerState
  case Map.lookup ann.name st.variables of
    Just s -> do
      ty <- M.instantiate s
      pure
        ( HLIR.MkExprVariable ann {HLIR.value = Identity ty},
          ty
        )
    Nothing -> throw (M.VariableNotFound ann.name)
synthesize (HLIR.MkExprLoc e p) = do
  HLIR.pushPosition p
  (e', ty) <- synthesize e
  void HLIR.popPosition
  pure (HLIR.MkExprLoc e' p, ty)
synthesize (HLIR.MkExprNative ann ty) = do
  let ty' = List.foldl (flip replaceIdwithQuVar) ty ann.value
  let scheme = HLIR.Forall ann.value ty'

  modifyIORef' M.checkerState $ \st -> st {M.variables = Map.insert ann.name scheme st.variables}

  pure (HLIR.MkExprNative ann ty', ty')
  where
    replaceIdwithQuVar :: Text -> HLIR.Type -> HLIR.Type
    replaceIdwithQuVar i (HLIR.MkTyId i')
      | i == i' =
          HLIR.MkTyQuantified i
    replaceIdwithQuVar i (HLIR.MkTyApp t1 t2) =
      HLIR.MkTyApp (replaceIdwithQuVar i t1) (replaceIdwithQuVar i <$> t2)
    replaceIdwithQuVar _ t = t
synthesize (HLIR.MkExprApplication f args) = do
  (f', ty) <- synthesize f
  case ty of
    argsTys HLIR.:->: retTy -> do
      when (length argsTys /= length args) $
        throw (M.InvalidArgumentQuantity (length argsTys) (length args))

      args' <- zipWithM check args argsTys
      pure (HLIR.MkExprApplication f' args', retTy)
    _ -> do
      (args', tys) <- unzip <$> traverse synthesize args

      ret <- M.fresh
      ty `U.unifiesWith` (tys HLIR.:->: ret)

      pure (HLIR.MkExprApplication f' args', ret)
synthesize (HLIR.MkExprLambda args ret body) = do
  args' <-
    traverse
      ( \(HLIR.MkAnnotation ann annTy) -> case annTy of
          Just ty -> pure (ann, ty)
          Nothing -> do
            ty <- M.fresh
            pure (ann, ty)
      )
      args

  ret' <- maybe M.fresh pure ret

  let schemes = map (second $ HLIR.Forall []) args'

  old <- readIORef M.checkerState <&> M.returnType
  modifyIORef' M.checkerState $ \st -> st {M.returnType = Just ret'}

  body' <- case ret of
    Just retTy -> do
      M.with
        M.checkerState
        (\st -> st {M.variables = Map.union (Map.fromList schemes) st.variables})
        $ check body retTy
    Nothing -> do
      M.with
        M.checkerState
        (\st -> st {M.variables = Map.union (Map.fromList schemes) st.variables})
        $ do
          (body', ret'') <- synthesize body
          ret' `U.unifiesWith` ret''
          pure body'

  modifyIORef' M.checkerState $ \st -> st {M.returnType = old}

  let wfArgs = map (uncurry HLIR.MkAnnotation . second Identity) args'

  pure (HLIR.MkExprLambda wfArgs (Identity ret') body', map snd args' HLIR.:->: ret')
synthesize (HLIR.MkExprLiteral l) = do
  let ty = case l of
        HLIR.MkLitChar _ -> HLIR.MkTyChar
        HLIR.MkLitInt _ -> HLIR.MkTyInt
        HLIR.MkLitFloat _ -> HLIR.MkTyFloat
        HLIR.MkLitString _ -> HLIR.MkTyString
        HLIR.MkLitBool _ -> HLIR.MkTyBool
  pure (HLIR.MkExprLiteral l, ty)
synthesize (HLIR.MkExprTernary c t e) = do
  c' <- check c HLIR.MkTyBool
  (t', thenTy) <- synthesize t
  e' <- check e thenTy

  pure (HLIR.MkExprTernary c' t' e', thenTy)
synthesize (HLIR.MkExprLet gens (Left ann) e b) | Just constrs <- getData e = do
  let name = ann.name
      generics = Set.toList gens
      header = if null generics then HLIR.MkTyId name else HLIR.MkTyApp (HLIR.MkTyId name) (HLIR.MkTyQuantified <$> generics)

  let schemes =
        map
          ( \case
              HLIR.MkDataConstructor constrName args ->
                (constrName, HLIR.Forall generics (args HLIR.:->: header))
              HLIR.MkDataVariable varName ->
                (varName, HLIR.Forall generics header)
          )
          constrs

  modifyIORef M.checkerState $ \st ->
    st
      { M.variables = st.variables,
        M.dataConstructors = Map.fromList schemes <> st.dataConstructors
      }

  let record = foldl' (\acc dat -> case dat of
        HLIR.MkDataConstructor n args -> HLIR.MkTyRowExtend n (args HLIR.:->: header) False acc
        HLIR.MkDataVariable n -> HLIR.MkTyRowExtend n header False acc
        ) HLIR.MkTyRowEmpty constrs

  modifyIORef M.checkerState $ \st ->
    st
      { M.variables = Map.insert name (HLIR.Forall generics (HLIR.MkTyRecord record)) st.variables }

  let e' = HLIR.MkExprData constrs

  (b', bTy) <- synthesize b

  pure (HLIR.MkExprLet gens (Left $ ann {HLIR.value = Identity record}) e' b', bTy)
synthesize (HLIR.MkExprLet generics (Right pattern) expr body) = do
  M.enterLevel

  (pat, patTy, env) <- typecheckPattern False pattern

  (expr', exprTy) <- synthesize expr

  exprTy `U.unifiesWith` patTy
  M.exitLevel

  newSchemes <- forM (Map.toList env) $ \(name, HLIR.Forall _ ty') -> do
    newScheme <-
      if null generics
        then M.generalize ty'
        else pure $ HLIR.Forall (toList generics) ty'

    modifyIORef' M.checkerState $ \st ->
      st {M.variables = Map.insert name newScheme st.variables}

    pure (name, newScheme)

  modifyIORef' M.checkerState $ \st -> st {M.variables = Map.fromList newSchemes <> st.variables }

  (body', bodyTy) <- synthesize body

  pure (HLIR.MkExprLet generics (Right pat) expr' body', bodyTy)
synthesize (HLIR.MkExprLet generics (Left ann) expr body) = do
  M.enterLevel
  ty <- maybe M.fresh pure ann.value
  let scheme = HLIR.Forall (toList generics) ty

  (expr', exprTy) <-
    M.with
      M.checkerState
      (\st -> st {M.variables = Map.insert ann.name scheme st.variables})
      $ synthesize expr

  exprTy `U.unifiesWith` ty
  M.exitLevel

  newScheme <-
    if null generics
      then M.generalize exprTy
      else pure $ HLIR.Forall (toList generics) exprTy

  modifyIORef' M.checkerState $ \st -> st {M.variables = Map.insert ann.name newScheme st.variables}

  (body', ty') <- synthesize body

  pure (HLIR.MkExprLet generics (Left $ ann {HLIR.value = Identity ty}) expr' body', ty')
synthesize (HLIR.MkExprMut expr) = do
  expectedTy <- M.fresh
  (expr', ty) <- synthesize expr

  expectedTy `U.unifiesWith` HLIR.MkTyMutable ty

  pure (HLIR.MkExprMut expr', HLIR.MkTyMutable ty)
synthesize (HLIR.MkExprBlock es) = do
  (es', tys) <- unzip <$> traverse synthesize es

  retTy <- maybe M.fresh pure (viaNonEmpty last tys)

  pure (HLIR.MkExprBlock es', retTy)
synthesize (HLIR.MkExprUpdate u e) = do
  (u', ty) <- typecheckUpdate u
  e' <- case ty of
    HLIR.MkTyMutable exprTy -> check e exprTy
    _ ->
      synthesize e >>= \case
        (e', ty') -> do
          ty `U.unifiesWith` HLIR.MkTyMutable ty'
          pure e'

  pure (HLIR.MkExprUpdate u' e', HLIR.MkTyUnit)
synthesize (HLIR.MkExprList es) = do
  (es', t) <- case uncons es of
    Just (hd, tl) -> do
      (hd', ty) <- synthesize hd
      tl' <- zipWithM check tl (repeat ty)

      pure (hd' : tl', HLIR.MkTyList ty)
    Nothing -> ([],) . HLIR.MkTyList <$> M.fresh
  pure (HLIR.MkExprList es', t)
synthesize (HLIR.MkExprWhile c e) = do
  c' <- check c HLIR.MkTyBool
  (e', _) <- synthesize e

  pure (HLIR.MkExprWhile c' e', HLIR.MkTyUnit)
synthesize (HLIR.MkExprIndex e i) = do
  tvar <- M.fresh

  e' <- check e (HLIR.MkTyList tvar)
  i' <- check i HLIR.MkTyInt

  pure (HLIR.MkExprIndex e' i', tvar)
synthesize (HLIR.MkExprMatch scrut cases) = do
  (scrut', scrutTy) <- synthesize scrut

  (_, cs) <-
    mapMWithAcc
      ( \acc (pat, e, p) -> do
          Foldable.for_ p HLIR.pushPosition
          (pat', patTy, env) <- typecheckPattern False pat
          scrutTy `U.unifiesWith` patTy
          Foldable.for_ p (const HLIR.popPosition)

          case acc of
            Just ty -> do
              e' <- M.with M.checkerState (\st -> st {M.variables = env <> st.variables}) $ check e ty

              pure (acc, ((pat', e', p), ty))
            Nothing -> do
              (e', eTy) <- M.with M.checkerState (\st -> st {M.variables = env <> st.variables}) $ synthesize e

              pure (Just eTy, ((pat', e', p), eTy))
      )
      Nothing
      cases

  let (cases', tys) = unzip cs

  (exprTy, exprTys) <- case tys of
    [] -> throw M.EmptyMatch
    (x : xs'') -> return (x, xs'')

  -- Unify the return type with the type of the case expressions
  forM_ exprTys $ U.unifiesWith exprTy

  pure (HLIR.MkExprMatch scrut' cases', exprTy)
synthesize (HLIR.MkExprData constrs) = do
  let record = foldl' (\acc dat -> case dat of
        HLIR.MkDataConstructor n args -> HLIR.MkTyRowExtend n (args HLIR.:->: HLIR.MkTyAbstractType) False acc
        HLIR.MkDataVariable n -> HLIR.MkTyRowExtend n HLIR.MkTyAbstractType False acc
        ) HLIR.MkTyRowEmpty constrs

  let schemes = map (\case
          HLIR.MkDataConstructor constrName args ->
            (constrName, HLIR.Forall [] (args HLIR.:->: HLIR.MkTyRecord record))
          HLIR.MkDataVariable varName ->
            (varName, HLIR.Forall [] (HLIR.MkTyRecord record))
        ) constrs

  modifyIORef' M.checkerState $ \st ->
    st {
      M.dataConstructors = Map.fromList schemes <> st.dataConstructors
    }

  pure (HLIR.MkExprData constrs, HLIR.MkTyRecord record)
synthesize (HLIR.MkExprRecordExtension e k opt v) = do
  (e', ty) <- synthesize e
  (v', vTy) <- synthesize v

  a <- M.fresh
  r <- M.fresh

  let funTy = [a, HLIR.MkTyRecord r] HLIR.:->: HLIR.MkTyRecord (HLIR.MkTyRowExtend k a opt r)

  ret <- M.fresh

  U.unifiesWith ([vTy, ty] HLIR.:->: ret) funTy

  pure (HLIR.MkExprRecordExtension e' k opt v', ret)
synthesize (HLIR.MkExprRecordAccess e k) = do
  (e', ty) <- synthesize e

  a <- M.fresh
  r <- M.fresh

  let funTy = [HLIR.MkTyRecord $ HLIR.MkTyRowExtend k a False r] HLIR.:->: a

  ret <- M.fresh

  U.unifiesWith ([ty] HLIR.:->: ret) funTy

  pure (HLIR.MkExprRecordAccess e' k, ret)
synthesize HLIR.MkExprRecordEmpty =
  pure (HLIR.MkExprRecordEmpty, HLIR.MkTyRecord HLIR.MkTyRowEmpty)
synthesize (HLIR.MkExprPublic e) = do
  (e', ty) <- synthesize e
  pure (HLIR.MkExprPublic e', ty)
synthesize (HLIR.MkExprRequire _ _) = compilerError "typecheck: require should not appear in typechecking"
synthesize (HLIR.MkExprSingleIf c t) = do
  c' <- check c HLIR.MkTyBool
  (t', ty) <- synthesize t

  pure (HLIR.MkExprSingleIf c' t', ty)
synthesize HLIR.MkExprBreak = do
  pure (HLIR.MkExprBreak, HLIR.MkTyUnit)
synthesize HLIR.MkExprContinue = do
  pure (HLIR.MkExprContinue, HLIR.MkTyUnit)
synthesize (HLIR.MkExprReturn e) = do
  (e', ty) <- synthesize e

  returnType <- readIORef M.checkerState <&> M.returnType
  case returnType of
    Just retTy -> do
      retTy `U.unifiesWith` ty
    Nothing -> do
      modifyIORef' M.checkerState $ \st -> st {M.returnType = Just ty}

  retTy <- M.fresh

  pure (HLIR.MkExprReturn e', retTy)
synthesize (HLIR.MkExprSpawn e) = do
  (e', ty) <- synthesize e

  retTy <- M.fresh
  U.unifiesWith ty ([] HLIR.:->: retTy)

  pure (HLIR.MkExprSpawn e', HLIR.MkTyApp (HLIR.MkTyId "Thread") [retTy])
synthesize (HLIR.MkExprTypeAlias ann t) = do
  let sch = HLIR.Forall ann.value t

  modifyIORef' M.checkerState $ \st -> st { M.typeAliases = Map.insert ann.name sch st.typeAliases }

  pure (HLIR.MkExprTypeAlias ann t, t)
synthesize (HLIR.MkExprInterface _ _) =
  compilerError "typecheck: interface should not appear in typechecking"

check :: (M.MonadChecker m) => HLIR.HLIR "expression" -> HLIR.Type -> m (HLIR.TLIR "expression")
check (HLIR.MkExprLambda args ret body) (argsTys HLIR.:->: retTy) = do
  args' <-
    zipWithM
      ( \(HLIR.MkAnnotation ann annTy) ty -> case annTy of
          Just ty' -> pure (ann, ty')
          Nothing -> pure (ann, ty)
      )
      args
      argsTys

  let schemes = map (second $ HLIR.Forall []) args'

  let retTy' = fromMaybe retTy ret

  old <- readIORef M.checkerState <&> M.returnType
  modifyIORef' M.checkerState $ \st -> st {M.returnType = Just retTy'}

  body' <-
    M.with
      M.checkerState
      (\st -> st {M.variables = Map.union (Map.fromList schemes) st.variables})
      $ check body retTy'

  modifyIORef' M.checkerState $ \st -> st {M.returnType = old}

  let wfArgs = map (uncurry HLIR.MkAnnotation . second Identity) args'

  pure (HLIR.MkExprLambda wfArgs (Identity retTy) body')
check (HLIR.MkExprLet generics (Left ann) expr body) ty = do
  M.enterLevel
  preExprTy <- M.fresh
  let scheme = HLIR.Forall (toList generics) preExprTy

  (expr', exprTy) <-
    M.with
      M.checkerState
      (\st -> st {M.variables = Map.insert ann.name scheme st.variables})
      $ synthesize expr

  preExprTy `U.unifiesWith` exprTy

  M.exitLevel

  newScheme <-
    if null generics
      then M.generalize ty
      else pure $ HLIR.Forall (toList generics) ty

  modifyIORef' M.checkerState $ \st -> st {M.variables = Map.insert ann.name newScheme st.variables}

  body' <- check body ty

  pure (HLIR.MkExprLet generics (Left $ ann {HLIR.value = Identity ty}) expr' body')
check (HLIR.MkExprMut expr) (HLIR.MkTyMutable ty) = do
  expr' <- check expr ty
  pure (HLIR.MkExprMut expr')
check (HLIR.MkExprBlock es) ty = do
  case unsnoc es of
    Just (initEs, lastE) -> do
      (initEs', _) <- unzip <$> traverse synthesize initEs
      lastE' <- check lastE ty
      pure (HLIR.MkExprBlock (initEs' <> [lastE']))
    Nothing -> pure (HLIR.MkExprBlock [])
check (HLIR.MkExprList es) (HLIR.MkTyList ty) = do
  es' <- traverse (`check` ty) es

  pure (HLIR.MkExprList es')
check (HLIR.MkExprMatch scrut cases) ty = do
  (scrut', scrutTy) <- synthesize scrut

  (cases', _) <-
    unzip
      <$> traverse
        ( \(pat, e, p) -> do
            Foldable.for_ p HLIR.pushPosition
            (pat', patTy, env) <- typecheckPattern False pat
            scrutTy `U.unifiesWith` patTy
            Foldable.for_ p (const HLIR.popPosition)

            e' <- M.with M.checkerState (\st -> st {M.variables = env <> st.variables}) $ check e ty

            pure ((pat', e', p), ty)
        )
        cases

  pure (HLIR.MkExprMatch scrut' cases')
check (HLIR.MkExprTernary c t e) ty = do
  c' <- check c HLIR.MkTyBool
  t' <- check t ty
  e' <- check e ty

  pure (HLIR.MkExprTernary c' t' e')
check (HLIR.MkExprWhile c e) ty = do
  c' <- check c HLIR.MkTyBool
  e' <- check e ty

  pure (HLIR.MkExprWhile c' e')
check (HLIR.MkExprIndex e i) ty = do
  e' <- check e (HLIR.MkTyList ty)
  i' <- check i HLIR.MkTyInt

  pure (HLIR.MkExprIndex e' i')
check (HLIR.MkExprLoc e p) ty = do
  HLIR.pushPosition p
  e' <- check e ty
  void HLIR.popPosition
  pure (HLIR.MkExprLoc e' p)
check (HLIR.MkExprVariable ann) ty = do
  st <- readIORef M.checkerState
  case Map.lookup ann.name st.variables of
    Just s -> do
      ty' <- M.instantiate s
      ty' `U.unifiesWith` ty
      pure (HLIR.MkExprVariable ann {HLIR.value = Identity ty})
    Nothing -> throw (M.VariableNotFound ann.name)
check (HLIR.MkExprLiteral l) ty = do
  let ty' = case l of
        HLIR.MkLitChar _ -> HLIR.MkTyChar
        HLIR.MkLitInt _ -> HLIR.MkTyInt
        HLIR.MkLitFloat _ -> HLIR.MkTyFloat
        HLIR.MkLitString _ -> HLIR.MkTyString
        HLIR.MkLitBool _ -> HLIR.MkTyBool
  ty' `U.unifiesWith` ty
  pure (HLIR.MkExprLiteral l)
check (HLIR.MkExprRecordAccess e k) ty = do
  r <- M.fresh

  e' <- check e (HLIR.MkTyRecord $ HLIR.MkTyRowExtend k ty False r)

  pure $ HLIR.MkExprRecordAccess e' k
check (HLIR.MkExprSingleIf c t) ty = do
  c' <- check c HLIR.MkTyBool
  t' <- check t ty

  pure (HLIR.MkExprSingleIf c' t')
check (HLIR.MkExprRequire _ _) _ = compilerError "typecheck: require should not appear in typechecking"
check (HLIR.MkExprPublic e) ty = HLIR.MkExprPublic <$> check e ty
check e ty = do
  (e', ty') <- synthesize e
  ty' `U.unifiesWith` ty
  pure e'

getData :: HLIR.HLIR "expression" -> Maybe [HLIR.DataConstructor HLIR.Type]
getData (HLIR.MkExprData constrs) = Just constrs
getData (HLIR.MkExprLoc e _) = getData e
getData _ = Nothing

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc xs = (,) <$> viaNonEmpty init xs <*> viaNonEmpty last xs

typecheckPattern :: (M.MonadChecker m) => Bool -> HLIR.HLIR "pattern" -> m (HLIR.TLIR "pattern", HLIR.Type, Map Text HLIR.Scheme)
typecheckPattern isInRecord (HLIR.MkPatVariable name varTy) = do
  st <- readIORef M.checkerState

  case Map.lookup name st.dataConstructors of
    Just s -> do
      ty' <- M.instantiate s
      pure ( 
          if isInRecord 
            then HLIR.MkPatVariable name (Identity ty')
            else HLIR.MkPatSpecial name, 
          ty', 
          Map.singleton name s
        )
    _ -> do
      ty <- maybe M.fresh pure varTy
      let scheme = HLIR.Forall [] ty
      pure (HLIR.MkPatVariable name (Identity ty), ty, Map.singleton name scheme)
typecheckPattern _ (HLIR.MkPatLiteral l) = do
  let ty = case l of
        HLIR.MkLitChar _ -> HLIR.MkTyChar
        HLIR.MkLitInt _ -> HLIR.MkTyInt
        HLIR.MkLitFloat _ -> HLIR.MkTyFloat
        HLIR.MkLitString _ -> HLIR.MkTyString
        HLIR.MkLitBool _ -> HLIR.MkTyBool
  pure (HLIR.MkPatLiteral l, ty, Map.empty)
typecheckPattern _ (HLIR.MkPatConstructor name pats) = do
  st <- readIORef M.checkerState
  case Map.lookup name st.dataConstructors of
    Just sch -> do
      ty <- M.instantiate sch
      case ty of
        tys HLIR.:->: ret -> do
          (pats', env) <-
            unzip
              <$> zipWithM
                ( \pat ty' -> do
                    (pat', ty'', env) <- typecheckPattern False pat
                    ty' `U.unifiesWith` ty''
                    pure (pat', env)
                )
                pats
                tys

          pure (HLIR.MkPatConstructor name pats', ret, Map.unions env)
        _ -> throw (M.InvalidConstructor name)
    _ -> throw (M.InvalidConstructor name)
typecheckPattern _ HLIR.MkPatWildcard = do
  ty <- M.fresh
  pure (HLIR.MkPatWildcard, ty, Map.empty)
typecheckPattern _ (HLIR.MkPatSpecial {}) =
  throw (CompilerError "typecheckPattern: special patterns are not supported")
typecheckPattern isInRecord (HLIR.MkPatLocated p loc) =
  HLIR.pushPosition loc *> typecheckPattern isInRecord p <* HLIR.popPosition
typecheckPattern _ (HLIR.MkPatOr p1 p2) = do
  (p1', ty, env1) <- typecheckPattern False p1
  (p2', ty', env2) <- typecheckPattern False p2

  ty `U.unifiesWith` ty'

  dcs <- readIORef M.checkerState <&> M.dataConstructors

  let env1' = Map.fromList $ filter (\(n, _) -> Map.notMember n dcs) (Map.toList env1)
      env2' = Map.fromList $ filter (\(n, _) -> Map.notMember n dcs) (Map.toList env2)

  instEnv1 <- traverse M.instantiate env1'
  instEnv2 <- traverse M.instantiate env2'

  -- Check if the environments are disjoint
  common <- intersectionWithM U.doesUnifyWith instEnv1 instEnv2

  if Map.size common /= Map.size env1' && Map.size common /= Map.size env2'
    then throw (M.InvalidPatternUnion (Map.keysSet env1') (Map.keysSet env2'))
    else
      if and common
        then pure (HLIR.MkPatOr p1' p2', ty, Map.union env1 env2)
        else throw (M.InvalidPatternUnion (Map.keysSet env1') (Map.keysSet env2'))
typecheckPattern _ (HLIR.MkPatCondition e p) = do
  (p', ty, env) <- typecheckPattern False p

  (e', ty') <- M.with M.checkerState (\st -> st {M.variables = env <> st.variables}) $ synthesize e

  ty' `U.unifiesWith` HLIR.MkTyBool

  pure (HLIR.MkPatCondition e' p', ty, env)
typecheckPattern _ (HLIR.MkPatList pats slice _) = do
  ty <- M.fresh

  (pats', tys, env) <- unzip3 <$> traverse (typecheckPattern False) pats

  forM_ tys $ U.unifiesWith ty

  case slice of
    Just p -> do
      (p', ty', env') <- typecheckPattern False p
      HLIR.MkTyList ty `U.unifiesWith` ty'
      pure (HLIR.MkPatList pats' (Just p') (Identity (HLIR.MkTyList ty)), HLIR.MkTyList ty, Map.unions env <> env')
    Nothing -> pure (HLIR.MkPatList pats' Nothing (Identity (HLIR.MkTyList ty)), HLIR.MkTyList ty, Map.unions env)
typecheckPattern isInRecord (HLIR.MkPatRecord pats) = do
  (pats', env) <- unzip <$> traverse (\(k, p) -> do
    (p', ty', env') <- typecheckPattern (isInRecord || True) p
    pure ((k, p', ty'), env'))
    (Map.toList pats)

  let env' = Map.unions env

  rest <- M.fresh

  let record = foldl' (\acc (k, _, p) -> HLIR.MkTyRowExtend k p False acc) rest pats'

  pure (HLIR.MkPatRecord (Map.fromList (map (\(k, p, _) -> (k, p)) pats')), HLIR.MkTyRecord record, env')


intersectionWithM :: (Ord k, Monad m) => (a -> b -> m c) -> Map k a -> Map k b -> m (Map k c)
intersectionWithM f m1 m2 = do
  let common = Map.intersection m1 m2
  Foldable.foldlM
    ( \m (k, v) -> do
        v' <- f v (m2 Map.! k)
        pure $ Map.insert k v' m
    )
    Map.empty
    (Map.toList common)

typecheckUpdate :: (M.MonadChecker m) => HLIR.HLIR "update" -> m (HLIR.TLIR "update", HLIR.Type)
typecheckUpdate (HLIR.MkUpdtVariable ann) = do
  st <- readIORef M.checkerState
  case Map.lookup ann.name st.variables of
    Just s -> do
      ty <- M.instantiate s
      pure (HLIR.MkUpdtVariable ann {HLIR.value = Identity ty}, ty)
    Nothing -> throw (M.VariableNotFound ann.name)
typecheckUpdate (HLIR.MkUpdtField _ _) = compilerError "typecheckUpdate: field updates are not supported"
typecheckUpdate (HLIR.MkUpdtIndex u e) = do
  elTy <- M.fresh
  (u', ty) <- typecheckUpdate u
  (e', idxTy) <- synthesize e

  idxTy `U.unifiesWith` HLIR.MkTyInt
  HLIR.MkTyList elTy `U.unifiesWith` ty

  pure (HLIR.MkUpdtIndex u' e', elTy)

runTypechecking :: (MonadIO m) => [HLIR.HLIR "expression"] -> m (Either M.Error [HLIR.TLIR "expression"])
runTypechecking es = do
  let st =
        M.MkCheckerState
          Map.empty
          mempty
          Nothing
          Map.empty

  writeIORef M.checkerState st
  writeIORef M.typeCounter 0
  writeIORef M.currentLevel 0

  res <- runExceptT $ traverse synthesize es

  pure $ case res of
    Left err -> Left err
    Right es' -> Right $ map fst es'

-- | Get the binding name of an expression
getName :: HLIR.HLIR "expression" -> Text
getName (HLIR.MkExprLet _ (Left ann) _ _) = ann.name
getName (HLIR.MkExprLoc e _) = getName e
getName e = compilerError $ "typecheck: event block should only contain let bindings or events, received " <> toText e

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

-- | Decomposing actor header into name and type arguments
-- | Used to lookup the interface of an actor.
decomposeHeader :: (M.MonadChecker m) => HLIR.Type -> m (Text, [HLIR.Type])
decomposeHeader (HLIR.MkTyActor t) = decomposeHeader t
decomposeHeader (HLIR.MkTyApp (HLIR.MkTyId name) tys) = pure (name, tys)
decomposeHeader (HLIR.MkTyId name) = pure (name, [])
decomposeHeader (HLIR.MkTyVar tv) = do
  tv' <- readIORef tv
  case tv' of
    HLIR.Link t -> decomposeHeader t
    HLIR.Unbound _ _ -> throw (M.InvalidHeader (HLIR.MkTyVar tv))
decomposeHeader t = M.throw $ M.InvalidHeader t

unrecord :: (MonadIO m) => HLIR.Type -> m HLIR.Type
unrecord (HLIR.MkTyVar tv) = do
  tv' <- readIORef tv
  case tv' of
    HLIR.Link t -> unrecord t
    HLIR.Unbound _ _ -> pure (HLIR.MkTyVar tv)
unrecord (HLIR.MkTyRecord t) = pure t
unrecord t = pure t
