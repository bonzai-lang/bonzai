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
  instantiateWithSub,
  fresh,
  generalize,
) where

import Control.Monad.Except
import Control.Monad.Result as R
import qualified GHC.IO as IO
import qualified Language.Bonzai.Syntax.Internal.Type as HLIR
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Set as Set

type MonadChecker m = (MonadIO m, MonadError Error m)
type Substitution = Map Text HLIR.Type

-- | Type counter is used to generate fresh type variables
{-# NOINLINE typeCounter #-}
typeCounter :: IORef Int
typeCounter = IO.unsafePerformIO $ newIORef 0

-- | Current level is used to keep track of the current level of the type variable
-- | in the typechecking process, in order to check for the scope of the type variable.
{-# NOINLINE currentLevel #-}
currentLevel :: IORef Int
currentLevel = IO.unsafePerformIO $ newIORef 0

data CheckerState = MkCheckerState {
    variables :: Map Text HLIR.Scheme
  , dataConstructors :: Map Text HLIR.Scheme
  , returnType :: Maybe HLIR.Type
  , typeAliases :: Map Text HLIR.Scheme
} deriving (Eq, Show)

-- | Helper function to update the state of the typechecker
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
    mempty
    Nothing
    Map.empty

enterLevel :: (MonadChecker m) => m ()
enterLevel = modifyIORef' currentLevel (+ 1)

exitLevel :: (MonadChecker m) => m ()
exitLevel = modifyIORef' currentLevel (\x -> x - 1)

genSymbol :: (MonadIO m) => m Text
genSymbol = do
  s <- readIORef typeCounter
  writeIORef typeCounter (s + 1)
  if s < 26
    then pure $ Text.singleton (chr (s + 97))
    else pure $ "t" <> show s

-- | Generate a fresh type variable
fresh :: (MonadIO m) => m HLIR.Type
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
    go subst (HLIR.MkTyApp (HLIR.MkTyId n) xs) = do
      typeAls <- readIORef checkerState <&> typeAliases

      case Map.lookup n typeAls of
        Just (HLIR.Forall qvars' ty') -> do
          sub <- Map.fromList <$> mapM (\x -> (x,) <$> fresh) qvars'
          let s' = Map.union sub subst
          (res, s2) <- go s' ty'
          (res', s3) <- goMany s2 xs
          pure (HLIR.MkTyApp res res', s3)
        Nothing -> do
          (xs', subst') <- goMany subst xs
          pure (HLIR.MkTyApp (HLIR.MkTyId n) xs', subst')
    go subst (HLIR.MkTyQuantified name) = case Map.lookup name subst of
      Just t -> pure (t, subst)
      Nothing -> pure (HLIR.MkTyQuantified name, subst)
    go subst (HLIR.MkTyId name) = case Map.lookup name subst of
      Just t -> pure (t, subst)
      Nothing -> do
        typeAls <- readIORef checkerState <&> typeAliases
        case Map.lookup name typeAls of
          Just (HLIR.Forall [] ty') -> go s ty'
          Just (HLIR.Forall qvars' _) -> throw $ R.InvalidArgumentQuantity 0 (length qvars')
          Nothing -> pure (HLIR.MkTyId name, subst)
    go subst (HLIR.MkTyApp t ts) = do
      (t', subst') <- go subst t
      (ts', subst'') <- goMany subst' ts
      pure (HLIR.MkTyApp t' ts', subst'')
    go subst (HLIR.MkTyVar ref) = do
      v <- readIORef ref
      case v of
        HLIR.Link t -> go subst t
        HLIR.Unbound name _ -> case Map.lookup name subst of
          Just t -> pure (t, subst)
          Nothing -> pure (HLIR.MkTyVar ref, subst)
    go subst (HLIR.MkTyRowExtend label fieldTy opt rowTail) = do
      (fieldTy', subst') <- go subst fieldTy
      (rowTail', subst'') <- go subst' rowTail
      pure (HLIR.MkTyRowExtend label fieldTy' opt rowTail', subst'')
    go subst HLIR.MkTyRowEmpty = pure (HLIR.MkTyRowEmpty, subst)
    go subst (HLIR.MkTyRecord r) = do
      (r', subst') <- go subst r
      pure (HLIR.MkTyRecord r', subst')
    go subst HLIR.MkTyAbstractType = pure (HLIR.MkTyAbstractType, subst)

    goMany :: (MonadChecker m) => Substitution -> [HLIR.Type] -> m ([HLIR.Type], Substitution)
    goMany subst (x : xs) = do
      (x', subst') <- go subst x
      (xs', subst'') <- goMany subst' xs
      pure (x' : xs', subst'')
    goMany subst [] = pure ([], subst)

-- | Generalize a type by quantifying over all free type variables
generalize :: (MonadChecker m) => HLIR.Type -> m HLIR.Scheme
generalize ty = do
  free <- getFreeVars ty

  pure $ HLIR.Forall (Set.toList free) ty
  where
    getFreeVars :: MonadChecker m => HLIR.Type -> m (Set Text)
    getFreeVars (HLIR.MkTyQuantified name) = pure $ Set.singleton name
    getFreeVars (HLIR.MkTyApp t ts) = do
      t' <- getFreeVars t
      ts' <- Set.unions <$> mapM getFreeVars ts
      pure $ t' <> ts'
    getFreeVars (HLIR.MkTyVar ref) = do
      v <- readIORef ref
      lvl <- readIORef currentLevel
      case v of
        HLIR.Link t -> getFreeVars t
        HLIR.Unbound name lvl' | lvl' > lvl -> pure $ Set.singleton name
        _ -> pure Set.empty
    getFreeVars (HLIR.MkTyRowExtend _ fieldTy _ rowTail) = do
      fieldTy' <- getFreeVars fieldTy
      rowTail' <- getFreeVars rowTail
      pure $ fieldTy' <> rowTail'
    getFreeVars HLIR.MkTyRowEmpty = pure Set.empty
    getFreeVars (HLIR.MkTyRecord r) = getFreeVars r
    getFreeVars _ = pure Set.empty

instance ToText a => ToText (Map Text a) where
  toText m = Text.intercalate ", " (map (\(k, v) -> Text.concat [k, ":", toText v]) (Map.toList m))
