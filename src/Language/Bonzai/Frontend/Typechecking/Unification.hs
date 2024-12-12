{-# LANGUAGE LambdaCase #-}
module Language.Bonzai.Frontend.Typechecking.Unification where

import qualified Language.Bonzai.Syntax.HLIR as HLIR
import qualified Language.Bonzai.Frontend.Typechecking.Monad as M
import qualified Data.Map as Map

-- Check to see if a TVar (the first argument) occurs in the type
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

-- Unify two types
-- Type unification is the process of making two types equal by
-- substituting type variables with other types.
-- The unification algorithm is based on the Hindley-Milner type
-- inference algorithm.
unifiesWith :: M.MonadChecker m => HLIR.Type -> HLIR.Type -> m ()
unifiesWith t t' = do
  t1 <- HLIR.simplify t
  t2 <- HLIR.simplify t'
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
      (HLIR.MkTyLive t1', HLIR.MkTyLive t2') -> unifiesWith t1' t2'
      (HLIR.MkTyLive t1', t2') -> unifiesWith t1' t2'
      (t1', HLIR.MkTyLive t2') -> unifiesWith t1' t2'
      (HLIR.MkTyId n, HLIR.MkTyId n') | n == n' -> pure ()
      _ -> M.throw (M.UnificationFail t1 t2)

-- | Check to see if two types can be unified without
-- | altering the types of any type variables.
doesUnifyWith :: M.MonadChecker m => HLIR.Type -> HLIR.Type -> m Bool
doesUnifyWith t t' = runExceptT (unifiesWith t t') >>= \case
  Left _ -> pure False
  Right _ -> pure True

-- | find function but in a monadic way
findM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findM f (x : xs) = do
  res <- f x
  case res of
    Just _ -> pure res
    Nothing -> findM f xs
findM _ [] = pure Nothing

-- | Find an interface by name and arguments
-- | The algorithm is as follows:
-- | 1. Get all interfaces from the checker state
-- | 2. Iterate over all interfaces and check if the name and
-- |    the number of arguments match
-- | 3. If they do, check if the arguments unify with the
-- |    arguments of the interface
-- | 4. If they do, return the interface
-- | 5. If no interface is found, return Nothing
findInterface :: (M.MonadChecker m) => Text -> [HLIR.Type] -> m (Maybe (Map Text HLIR.Scheme))
findInterface name args = do
  interfaces <- Map.toList . M.interfaces <$> readIORef M.checkerState

  findM (\((name', args'), m) -> do
    if name == name' && length args == length args'
      then do
        b <- and <$> zipWithM doesUnifyWith args args'
        
        pure $ if b then Just m else Nothing
      else pure Nothing
    ) interfaces

  