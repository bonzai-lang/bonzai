{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Bonzai.Frontend.Typechecking.Monad (
  module R,
  MonadChecker,
  CheckerState(..),
  
  typeCounter,
  checkerState,
  with,

  currentLevel,
  enterLevel,
  exitLevel,

  instantiate,
  fresh,
) where

import Control.Monad.Except
import Control.Monad.Result as R
import qualified GHC.IO as IO
import qualified Language.Bonzai.Syntax.Internal.Type as HLIR
import qualified Data.Map as Map
import qualified Data.Text as Text

type MonadChecker m = (MonadIO m, MonadError Error m)
type Substitution = Map Text HLIR.Type

{-# NOINLINE typeCounter #-}
typeCounter :: IORef Int
typeCounter = IO.unsafePerformIO $ newIORef 0

{-# NOINLINE currentLevel #-}
currentLevel :: IORef Int
currentLevel = IO.unsafePerformIO $ newIORef 0

data CheckerState = MkCheckerState {
    variables :: Map Text HLIR.Scheme
  , interfaces :: Map Text (Map Text HLIR.Type)
}

with :: MonadIO m => IORef a -> (a -> a) -> m b -> m b
with ref f m = do
  x <- readIORef ref
  writeIORef ref (f x)
  res <- m
  writeIORef ref x
  pure res

{-# NOINLINE checkerState #-}
checkerState :: IORef CheckerState
checkerState = IO.unsafePerformIO . newIORef $ 
  MkCheckerState 
    Map.empty
    Map.empty

enterLevel :: (MonadChecker m) => m ()
enterLevel = modifyIORef' currentLevel (+ 1)

exitLevel :: (MonadChecker m) => m ()
exitLevel = modifyIORef' currentLevel (\x -> x - 1)

genSymbol :: (MonadChecker m) => m Text
genSymbol = do
  s <- readIORef typeCounter
  writeIORef typeCounter (s + 1)
  if s < 26
    then pure $ Text.singleton (chr (s + 97))
    else pure $ "t" <> show s

-- | Generate a fresh type variable
fresh :: (MonadChecker m) => m HLIR.Type
fresh = do
  s <- genSymbol
  lvl <- readIORef currentLevel
  ref <- newIORef (HLIR.Unbound s lvl)
  pure $ HLIR.MkTyVar ref


instantiate :: (MonadChecker m) => HLIR.Scheme -> m HLIR.Type
instantiate t = fst <$> instantiateWithSub mempty t

-- | instantiation: replace schematic variables with fresh TVar
instantiateWithSub :: (MonadChecker m) => Substitution -> HLIR.Scheme -> m (HLIR.Type, Substitution)
instantiateWithSub s (HLIR.Forall qvars ty) = do
  sub <- Map.fromList <$> mapM (\x -> (x,) <$> fresh) qvars
  let s' = Map.union sub s
  (res, s2) <- go s' ty
  pure (res, s2)
  where
    go :: (MonadChecker m) => Substitution -> HLIR.Type -> m (HLIR.Type, Substitution)
    go subst (HLIR.MkTyQuantified name) = case Map.lookup name subst of
      Just t -> pure (t, subst)
      Nothing -> pure (HLIR.MkTyQuantified name, subst)
    go subst (HLIR.MkTyApp t ts) = do
      (t', subst') <- go subst t
      (ts', subst'') <- goMany subst' ts
      pure (HLIR.MkTyApp t' ts', subst'')
    go subst (HLIR.MkTyVar ref) = do
      v <- readIORef ref
      case v of
        HLIR.Link t -> go subst t
        HLIR.Unbound _ _ -> pure (HLIR.MkTyVar ref, subst)
    go subst t = pure (t, subst)

    goMany :: (MonadChecker m) => Substitution -> [HLIR.Type] -> m ([HLIR.Type], Substitution)
    goMany subst (x : xs) = do
      (x', subst') <- go subst x
      (xs', subst'') <- goMany subst' xs
      pure (x' : xs', subst'')
    goMany subst [] = pure ([], subst)

instance ToText a => ToText (Map Text a) where
  toText m = Text.intercalate ", " (map (\(k, v) -> Text.concat [k, ":", toText v]) (Map.toList m))
