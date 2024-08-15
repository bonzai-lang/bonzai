module Language.Bonzai.Backend.Bytecode.Conversion where
import qualified GHC.IO as IO
import qualified Language.Bonzai.Syntax.Bytecode as BC
import qualified Data.Map as Map
import qualified Language.Bonzai.Syntax.LLIR as LLIR
import qualified Data.Set as Set

{-# NOINLINE localPool #-}
localPool :: IORef (Map Text Int)
localPool = IO.unsafePerformIO $ newIORef mempty

{-# NOINLINE globalPool #-}
globalPool :: IORef (Map Text Int)
globalPool = IO.unsafePerformIO $ newIORef mempty

class Assemble a where
  assemble :: MonadIO m => a -> m [BC.Instruction]

instance Assemble a => Assemble [a] where
  assemble = fmap concat . mapM assemble

instance Assemble LLIR.Instruction where
  assemble (LLIR.LoadLocal n) = do
    locals <- readIORef localPool
    case Map.lookup n locals of
      Just address -> do
        address' <- negIdx address
        pure [BC.LoadLocal address']
      Nothing -> error $ "Local " <> n <> " not found"
    
  assemble (LLIR.StoreLocal name) = do
    locals <- readIORef localPool
    case Map.lookup name locals of
      Just address -> do
        address' <- negIdx address
        pure [BC.StoreLocal address']
      Nothing -> error $ "Local " <> name <> " not found"

  assemble (LLIR.LoadConstant address) = pure [BC.LoadConstant address]

  assemble (LLIR.LoadGlobal address) = do
    globals <- readIORef globalPool
    case Map.lookup address globals of
      Just address' -> pure [BC.LoadGlobal address']
      Nothing -> error $ "Global " <> address <> " not found"
  
  assemble (LLIR.StoreGlobal address) = do
    globals <- readIORef globalPool
    case Map.lookup address globals of
      Just address' -> pure [BC.StoreGlobal address']
      Nothing -> error $ "Global " <> address <> " not found"
  
  assemble (LLIR.LoadNative name) = pure [BC.LoadNative name]

  assemble LLIR.Update = pure [BC.Update]
  assemble LLIR.Return = pure [BC.Return]
  assemble (LLIR.Compare cmp) = pure [BC.Compare cmp]
  assemble (LLIR.MakeList size) = pure [BC.MakeList size]
  assemble (LLIR.ListGet index) = pure [BC.ListGet index]

  assemble (LLIR.Call arity) = pure [BC.Call arity]
  assemble (LLIR.CallGlobal index arity) = do
    globals <- readIORef globalPool
    case Map.lookup index globals of
      Just idx -> pure [BC.CallGlobal idx arity]
      Nothing -> error $ "Global " <> index <> " not found"
  assemble (LLIR.CallLocal index arity) = do
    locals <- readIORef localPool
    case Map.lookup index locals of
      Just idx -> do
        addr <- negIdx idx
        pure [BC.CallLocal addr arity]
      Nothing -> error $ "Local " <> index <> " not found"
  
  assemble (LLIR.JumpIfFalse address) = pure [BC.JumpIfFalse address]
  assemble (LLIR.JumpRel address) = pure [BC.JumpRel address]
  assemble LLIR.GetIndex = pure [BC.GetIndex]
  assemble LLIR.Special = pure [BC.Special]
  assemble LLIR.Halt = pure [BC.Halt]

  assemble (LLIR.MakeEvent eq lq len _) = pure [BC.MakeEvent eq lq len]
  assemble (LLIR.EventOn event args body) = pure [BC.EventOn event args body]
  assemble (LLIR.Send args body) = pure [BC.Send args body]
  assemble LLIR.Spawn = pure [BC.Spawn]
  assemble LLIR.MakeMutable = pure [BC.MakeMutable]

instance Assemble LLIR.Segment where
  assemble (LLIR.Function name _ ls freed instructions) = do
    let freed' = Map.fromList freed

    globals <- readIORef globalPool
    case Map.lookup name globals of
      Just addr -> do
        instructions' <- withLocals freed' $ assemble instructions
        pure (BC.MakeFunctionAndStore addr (length instructions') ls : instructions')
      
      Nothing -> error $ "Global " <> name <> " not found"

  assemble (LLIR.Event name _ ls instructions eq lq) = do
    let freed' = Map.fromList ls

    globals <- readIORef globalPool
    case Map.lookup name globals of
      Just addr -> do
        instructions' <- withLocals freed' $ assemble instructions
        pure (BC.MakeEvent eq lq (length instructions' + 1) : instructions' <> [BC.ReturnEvent, BC.StoreGlobal addr])
      
      Nothing -> error $ "Global " <> name <> " not found"

  assemble (LLIR.Instruction instruction) = assemble instruction

negIdx :: MonadIO m => Int -> m Int
negIdx i = do
  locals <- readIORef localPool
  pure $ negate (Map.size locals) + i

withLocals :: MonadIO m => Map Text Int -> m a -> m a
withLocals locals act = do
  localPool' <- readIORef localPool
  writeIORef localPool locals
  r <- act
  writeIORef localPool localPool'
  pure r

fromMap :: Ord k => Map k a -> Set k
fromMap = Set.fromList . Map.keys

runBytecodeConversion :: MonadIO m => Set Text -> [LLIR.Segment] -> m [BC.Instruction]
runBytecodeConversion globals xs = do
  let globals' = Map.fromList . (`zip` [0..]) $ Set.toList globals

  writeIORef globalPool globals'

  res <- assemble xs

  pure $ res <> [BC.Halt]