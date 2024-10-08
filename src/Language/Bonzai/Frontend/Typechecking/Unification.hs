{-# LANGUAGE LambdaCase #-}
module Language.Bonzai.Frontend.Typechecking.Unification where

import qualified Language.Bonzai.Syntax.HLIR as HLIR
import qualified Language.Bonzai.Frontend.Typechecking.Monad as M
import qualified Data.Map as Map

-- check to see if a TVar (the first argument) occurs in the type
-- given as the second argument. Fail if it does.
-- At the same time, update the levels of all encountered free
-- variables to be the min of variable's current level and
-- the level of the given variable tvr.
doesOccurB :: IORef HLIR.TyVar -> HLIR.Type -> IO Bool
doesOccurB tvr (HLIR.MkTyVar tv') = do
  tvr' <- readIORef tvr
  tvr'' <- readIORef tv'
  case tvr'' of
    HLIR.Link t -> doesOccurB tvr t
    HLIR.Unbound name lvl -> do
      let newMinLvl = case tvr' of
            HLIR.Link _ -> lvl
            HLIR.Unbound _ lvl' -> min lvl' lvl
      writeIORef tv' (HLIR.Unbound name newMinLvl)
      pure (tvr == tv')
doesOccurB tv (HLIR.MkTyApp t1 t2) = do
  b <- doesOccurB tv t1
  if b
    then pure True
    else or <$> traverse (doesOccurB tv) t2
doesOccurB _ _ = pure False

-- unify two types
unifiesWith :: M.MonadChecker m => HLIR.Type -> HLIR.Type -> m ()
unifiesWith t t' = do
  t1 <- liftIO $ compressPaths t
  t2 <- liftIO $ compressPaths t'
  if t1 == t2
    then pure ()
    else case (t1, t2) of
      (HLIR.MkTyVar tv1, _) -> readIORef tv1 >>= \case
        HLIR.Link tl -> unifiesWith tl t2
        HLIR.Unbound _ _ -> do
          whenM (liftIO $ doesOccurB tv1 t2) $ do
            M.throw (M.UnificationFail t1 t2)
          writeIORef tv1 (HLIR.Link t2)
      (_, HLIR.MkTyVar _) -> unifiesWith t2 t1
      (HLIR.MkTyApp t1a t1b, HLIR.MkTyApp t2a t2b) | length t1b == length t2b -> do
        unifiesWith t1a t2a
        zipWithM_ unifiesWith t1b t2b
      (HLIR.MkTyId n, HLIR.MkTyId n') | n == n' -> pure ()
      _ -> M.throw (M.UnificationFail t1 t2)

compressPaths' :: MonadIO m => [HLIR.Type] -> m [HLIR.Type]
compressPaths' = traverse compressPaths

compressPaths :: MonadIO m => HLIR.Type -> m HLIR.Type
compressPaths (HLIR.MkTyVar tv) = do
  tv' <- readIORef tv
  case tv' of
    HLIR.Link t -> do
      t' <- compressPaths t
      writeIORef tv (HLIR.Link t')
      pure t'
    HLIR.Unbound _ _ -> pure (HLIR.MkTyVar tv)
compressPaths (HLIR.MkTyApp t ts) = do
  t' <- compressPaths t
  ts' <- traverse compressPaths ts
  pure (HLIR.MkTyApp t' ts')
compressPaths t = pure t

association :: Ord a => Map a b -> Map a c -> Map a (Maybe b, Maybe c)
association m1 m2 = Map.fromList $ do
  (k, v) <- Map.toList m1
  pure (k, (Just v, Map.lookup k m2))