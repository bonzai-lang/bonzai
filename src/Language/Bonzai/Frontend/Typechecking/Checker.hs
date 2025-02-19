{-# LANGUAGE LambdaCase #-}
module Language.Bonzai.Frontend.Typechecking.Checker where

import qualified Language.Bonzai.Syntax.HLIR as HLIR
import qualified Language.Bonzai.Frontend.Typechecking.Monad as M
import Control.Monad.Result
import qualified Data.Map as Map
import qualified Language.Bonzai.Frontend.Typechecking.Unification as U
import qualified Data.List as List
import qualified Data.Foldable as Foldable
import qualified Data.Set as Set

mapWithAcc :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapWithAcc _ acc [] = (acc, [])
mapWithAcc f acc (x:xs) = 
  let (acc', x') = f acc x
      (acc'', xs') = mapWithAcc f acc' xs
  in (acc'', x':xs')

mapMWithAcc :: Monad m => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
mapMWithAcc _ acc [] = pure (acc, [])
mapMWithAcc f acc (x:xs) = do
  (acc', x') <- f acc x
  (acc'', xs') <- mapMWithAcc f acc' xs
  pure (acc'', x':xs')

-- | Typecheck an expression : misworded, should be infer the type of an expression
-- | as bi-directional typechecking is not supported.
synthesize :: M.MonadChecker m => HLIR.HLIR "expression" -> m (HLIR.TLIR "expression", HLIR.Type)
synthesize (HLIR.MkExprVariable ann) = do
  st <- readIORef M.checkerState
  case Map.lookup ann.name st.variables of
    Just s -> do
      ty <- M.instantiate s
      pure (
          HLIR.MkExprVariable ann { HLIR.value = Identity ty }, 
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

  pos <- HLIR.peekPosition'

  modifyIORef' M.checkerState $ \st -> st { M.variables = Map.insert ann.name scheme st.variables, M.varPos = (ann.name, (scheme, pos)) : M.varPos st }

  pure (HLIR.MkExprNative ann ty', ty')

  where replaceIdwithQuVar :: Text -> HLIR.Type -> HLIR.Type
        replaceIdwithQuVar i (HLIR.MkTyId i') | i == i'
          = HLIR.MkTyQuantified i
        replaceIdwithQuVar i (HLIR.MkTyApp t1 t2)
          = HLIR.MkTyApp (replaceIdwithQuVar i t1) (replaceIdwithQuVar i <$> t2)
        replaceIdwithQuVar _ t = t
synthesize (HLIR.MkExprApplication f args) = do
  (f', ty) <- synthesize f
  case ty of
    argsTys HLIR.:->: retTy -> do
      args' <- zipWithM check args argsTys
      pure (HLIR.MkExprApplication f' args', retTy)
    _ -> do
      (args', tys) <- unzip <$> traverse synthesize args

      ret <- M.fresh
      ty `U.unifiesWith` (tys HLIR.:->: ret)

      pure (HLIR.MkExprApplication f' args', ret)

synthesize (HLIR.MkExprLambda args ret body) = do
  args' <- traverse (\(HLIR.MkAnnotation ann annTy) -> case annTy of
      Just ty -> pure (ann, ty)
      Nothing -> do
        ty <- M.fresh
        pure (ann, ty)
    ) args

  ret' <- maybe M.fresh pure ret

  let schemes = map (second $ HLIR.Forall []) args'

  body' <- case ret of
    Just retTy -> do
      M.with
        M.checkerState
        (\st -> st { M.variables = Map.union (Map.fromList schemes) st.variables })
        $ check body retTy
    Nothing -> do
      M.with
        M.checkerState
        (\st -> st { M.variables = Map.union (Map.fromList schemes) st.variables })
        $ do
          (body', ret'') <- synthesize body
          ret' `U.unifiesWith` ret''
          pure body'

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
synthesize (HLIR.MkExprLet generics ann expr) = do
  M.enterLevel
  ty <- M.fresh
  let scheme = HLIR.Forall (toList generics) ty

  (expr', exprTy) <- M.with
    M.checkerState
    (\st -> st { M.variables = Map.insert ann.name scheme st.variables })
    $ synthesize expr

  ty `U.unifiesWith` exprTy
  M.exitLevel

  newScheme <- if null generics
    then M.generalize ty
    else pure $ HLIR.Forall (toList generics) ty

  pos <- HLIR.peekPosition'

  modifyIORef' M.checkerState $ \st -> st { M.variables = Map.insert ann.name newScheme st.variables, M.varPos = (ann.name, (newScheme, pos)) : M.varPos st }

  pure (HLIR.MkExprLet generics ann { HLIR.value = Identity ty } expr', HLIR.MkTyUnit)
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
    _ -> throw M.InvalidUpdate

  pure (HLIR.MkExprUpdate u' e', HLIR.MkTyUnit)
synthesize (HLIR.MkExprList es) = do
  (es', t) <- case uncons es of
    Just (hd, tl) -> do
      (hd', ty) <- synthesize hd
      tl' <- zipWithM check tl (repeat ty)

      pure (hd': tl', HLIR.MkTyList ty)
    Nothing ->  ([],) . HLIR.MkTyList <$> M.fresh
  pure (HLIR.MkExprList es', t)
synthesize (HLIR.MkExprInterface ann defs) = do
  let name = ann.name
      generics = ann.value

  let actor = if null ann.value then (name, []) else (name, generics)

  let schemes = map (\(HLIR.MkAnnotation funName funTy) -> (funName, funTy)) defs

  modifyIORef M.checkerState $ \st -> st { M.interfaces = Map.insert actor (Map.fromList schemes) st.interfaces }

  pure (HLIR.MkExprInterface ann defs, HLIR.MkTyId name)
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

  (_, cs) <- mapMWithAcc (\acc (pat, e, pos) -> do
      HLIR.pushPosition pos
      (pat', patTy, env) <- typecheckPattern pat
      scrutTy `U.unifiesWith` patTy

      case acc of
        Just ty -> do
          e' <- M.with M.checkerState (\st -> st { M.variables = env <> st.variables }) $ check e ty
          
          void HLIR.popPosition
          
          pure (acc, ((pat', e', pos), ty))
        Nothing -> do
          (e', eTy) <- M.with M.checkerState (\st -> st { M.variables = env <> st.variables }) $ synthesize e

          void HLIR.popPosition

          pure (Just eTy, ((pat', e', pos), eTy))
    ) Nothing cases

  let (cases', tys) = unzip cs

  (exprTy, exprTys) <- case tys of
    [] -> throw M.EmptyMatch
    (x : xs'') -> return (x, xs'')

  -- Unify the return type with the type of the case expressions
  forM_ exprTys $ U.unifiesWith exprTy

  pure (HLIR.MkExprMatch scrut' cases', exprTy)

synthesize (HLIR.MkExprData ann constrs) = do
  let name = ann.name
      generics = ann.value
      header = if null generics then HLIR.MkTyId name else HLIR.MkTyApp (HLIR.MkTyId name) (HLIR.MkTyQuantified <$> generics)

  let schemes = map (\case
          HLIR.MkDataConstructor constrName args ->
            (constrName, HLIR.Forall generics (args HLIR.:->: header))

          HLIR.MkDataVariable varName ->
            (varName, HLIR.Forall generics header)
        ) constrs

  modifyIORef M.checkerState $ \st -> st { 
    M.variables = Map.fromList schemes <> st.variables 
  , M.dataConstructors = Set.fromList (map fst schemes) <> st.dataConstructors
  }

  pure (HLIR.MkExprData ann constrs, HLIR.MkTyId name)
synthesize (HLIR.MkExprPublic e) = synthesize e
synthesize (HLIR.MkExprRequire _ _) = compilerError "typecheck: require should not appear in typechecking"
synthesize (HLIR.MkExprModule _ _) = compilerError "typecheck: module should not appear in typechecking"

check :: M.MonadChecker m => HLIR.HLIR "expression" -> HLIR.Type -> m (HLIR.TLIR "expression")
check (HLIR.MkExprApplication f args) fTy@(argsTys HLIR.:->: _) = do
  f' <- check f fTy
  args' <- zipWithM check args argsTys
  
  pure (HLIR.MkExprApplication f' args')
check (HLIR.MkExprLambda args ret body) (argsTys HLIR.:->: retTy) = do
  args' <- zipWithM (\(HLIR.MkAnnotation ann annTy) ty -> case annTy of
      Just ty' -> pure (ann, ty')
      Nothing -> pure (ann, ty)
    ) args argsTys

  let schemes = map (second $ HLIR.Forall []) args'

  let retTy' = fromMaybe retTy ret

  body' <- M.with
    M.checkerState
    (\st -> st { M.variables = Map.union (Map.fromList schemes) st.variables })
    $ check body retTy'

  let wfArgs = map (uncurry HLIR.MkAnnotation . second Identity) args'

  pure (HLIR.MkExprLambda wfArgs (Identity retTy) body')
check (HLIR.MkExprLet generics ann expr) ty = do
  M.enterLevel
  let scheme = HLIR.Forall (toList generics) ty

  expr' <- M.with
    M.checkerState
    (\st -> st { M.variables = Map.insert ann.name scheme st.variables })
    $ check expr ty

  M.exitLevel

  newScheme <- if null generics
    then M.generalize ty
    else pure $ HLIR.Forall (toList generics) ty

  pos <- HLIR.peekPosition'

  modifyIORef' M.checkerState $ \st -> st { M.variables = Map.insert ann.name newScheme st.variables, M.varPos = (ann.name, (newScheme, pos)) : M.varPos st }

  pure (HLIR.MkExprLet generics ann { HLIR.value = Identity ty } expr')
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
check (HLIR.MkExprUpdate u e) ty = do
  (u', _) <- typecheckUpdate u
  e' <- check e ty
  pure (HLIR.MkExprUpdate u' e')
check (HLIR.MkExprList es) (HLIR.MkTyList ty) = do
  es' <- traverse (`check` ty) es

  pure (HLIR.MkExprList es')
check (HLIR.MkExprMatch scrut cases) ty = do
  (scrut', scrutTy) <- synthesize scrut

  (cases', _) <- unzip <$> traverse (\(pat, e, pos) -> do
      HLIR.pushPosition pos
      (pat', patTy, env) <- typecheckPattern pat
      scrutTy `U.unifiesWith` patTy

      e' <- M.with M.checkerState (\st -> st { M.variables = env <> st.variables }) $ check e ty

      void HLIR.popPosition

      pure ((pat', e', pos), ty)
    ) cases

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
      pure (HLIR.MkExprVariable ann { HLIR.value = Identity ty })
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
check (HLIR.MkExprRequire _ _) _ = compilerError "typecheck: require should not appear in typechecking"
check (HLIR.MkExprModule _ _) _ = compilerError "typecheck: module should not appear in typechecking"
check (HLIR.MkExprPublic e) ty = check e ty
check e ty = do
  (e', ty') <- synthesize e
  ty' `U.unifiesWith` ty
  pure e'

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc xs = (,) <$> viaNonEmpty init xs <*> viaNonEmpty last xs

typecheckPattern :: M.MonadChecker m => HLIR.HLIR "pattern" -> m (HLIR.TLIR "pattern", HLIR.Type, Map Text HLIR.Scheme)
typecheckPattern (HLIR.MkPatVariable name varTy) = do
  st <- readIORef M.checkerState

  case Map.lookup name st.variables of
    Just s | name `Set.member` st.dataConstructors -> do
      ty' <- M.instantiate s
      pure (HLIR.MkPatSpecial name, ty', Map.empty)
    _ -> do
      ty <- maybe M.fresh pure varTy
      let scheme = HLIR.Forall [] ty
      pure (HLIR.MkPatVariable name (Identity ty), ty, Map.singleton name scheme)
typecheckPattern (HLIR.MkPatLiteral l) = do
  let ty = case l of
        HLIR.MkLitChar _ -> HLIR.MkTyChar
        HLIR.MkLitInt _ -> HLIR.MkTyInt
        HLIR.MkLitFloat _ -> HLIR.MkTyFloat
        HLIR.MkLitString _ -> HLIR.MkTyString
        HLIR.MkLitBool _ -> HLIR.MkTyBool
  pure (HLIR.MkPatLiteral l, ty, Map.empty)
typecheckPattern (HLIR.MkPatConstructor name pats) = do
  st <- readIORef M.checkerState
  case Map.lookup name st.variables of
    Just sch | name `Set.member` st.dataConstructors -> do
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
    _ -> throw (M.InvalidConstructor name)
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

  (e', ty') <- M.with M.checkerState (\st -> st { M.variables = env <> st.variables }) $ synthesize e

  ty' `U.unifiesWith` HLIR.MkTyBool

  pure (HLIR.MkPatCondition e' p', ty, env)
typecheckPattern (HLIR.MkPatList pats slice _) = do
  ty <- M.fresh

  (pats', tys, env) <- unzip3 <$> traverse typecheckPattern pats

  forM_ tys $ U.unifiesWith ty

  case slice of
    Just p -> do
      (p', ty', env') <- typecheckPattern p
      HLIR.MkTyList ty `U.unifiesWith` ty'
      pure (HLIR.MkPatList pats' (Just p') (Identity (HLIR.MkTyList ty)), HLIR.MkTyList ty, Map.unions env <> env')
    Nothing -> pure (HLIR.MkPatList pats' Nothing (Identity (HLIR.MkTyList ty)), HLIR.MkTyList ty, Map.unions env)

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
  (e', idxTy) <- synthesize e

  idxTy `U.unifiesWith` HLIR.MkTyInt
  HLIR.MkTyList elTy `U.unifiesWith` ty

  pure (HLIR.MkUpdtIndex u' e', elTy)

runTypechecking :: (MonadIO m) => [HLIR.HLIR "expression"] -> m (Either M.Error [HLIR.TLIR "expression"])
runTypechecking es = do
  let st =
        M.MkCheckerState
          Map.empty
          Map.empty
          mempty
          mempty
  writeIORef M.checkerState st
  writeIORef M.typeCounter 0
  writeIORef M.currentLevel 0
  res <- M.with M.checkerState (const st) $ runExceptT $ traverse synthesize es
  pure $ case res of
    Left err -> Left err
    Right es' -> Right $ map fst es'

-- | Get the binding name of an expression
getName :: HLIR.HLIR "expression" -> Text
getName (HLIR.MkExprLet _ ann _) = ann.name
getName (HLIR.MkExprLoc e _) = getName e
getName e = compilerError $ "typecheck: event block should only contain let bindings or events, received " <> toText e

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

-- | Decomposing actor header into name and type arguments
-- | Used to lookup the interface of an actor.
decomposeHeader :: M.MonadChecker m => HLIR.Type -> m (Text, [HLIR.Type])
decomposeHeader (HLIR.MkTyActor t) = decomposeHeader t
decomposeHeader (HLIR.MkTyApp (HLIR.MkTyId name) tys) = pure (name, tys)
decomposeHeader (HLIR.MkTyId name) = pure (name, [])
decomposeHeader (HLIR.MkTyVar tv) = do
  tv' <- readIORef tv
  case tv' of
    HLIR.Link t -> decomposeHeader t
    HLIR.Unbound _ _ -> throw (M.InvalidHeader (HLIR.MkTyVar tv))
decomposeHeader t = M.throw $ M.InvalidHeader t