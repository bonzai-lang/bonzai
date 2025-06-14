module Language.Bonzai.Backend.Bytecode.Conversion where
import qualified GHC.IO as IO
import qualified Language.Bonzai.Syntax.Bytecode as BC
import qualified Data.Map as Map
import qualified Language.Bonzai.Syntax.LLIR as LLIR
import qualified Data.Set as Set
import Control.Monad.Result (compilerError)

-- | BYTECODE ASSEMBLER
-- | The bytecode assembler is the process of transforming the LLIR bytecode
-- | AST into a sequence of instructions that can be read by the virtual machine.
-- | This step is not that much different from the previous one, it globally only
-- | requires to set-up correctly load indexes and call indexes, by removing the
-- | variables names in favor of negative indexes.
-- |
-- | We're using negative indexes for optimization purposes, as it's easier to 
-- | lookup when only knowing the base pointer in the execution process.

{-# NOINLINE localPool #-}
localPool :: IORef (Map Text Int)
localPool = IO.unsafePerformIO $ newIORef mempty

{-# NOINLINE currentContinueAddress #-}
currentContinueAddress :: IORef (Maybe Int)
currentContinueAddress = IO.unsafePerformIO $ newIORef Nothing

{-# NOINLINE currentBreakAddress #-}
currentBreakAddress :: IORef (Maybe Int)
currentBreakAddress = IO.unsafePerformIO $ newIORef Nothing

{-# NOINLINE globalPool #-}
globalPool :: IORef (Map Text Int)
globalPool = IO.unsafePerformIO $ newIORef mempty

{-# NOINLINE currentIPC #-}
currentIPC :: IORef Int
currentIPC = IO.unsafePerformIO $ newIORef 0

class Assemble a where
  assemble :: MonadIO m => a -> m [BC.Instruction]

increase :: MonadIO m => m a -> m a
increase act = do
  modifyIORef' currentIPC (+ 1)
  act

instance Assemble a => Assemble [a] where
  assemble = fmap concat . mapM assemble

instance Assemble LLIR.Instruction where
  assemble (LLIR.LoadLocal n) = increase $ do
    locals <- readIORef localPool
    case Map.lookup n locals of
      Just address -> do
        address' <- negIdx address
        pure [BC.LoadLocal address']
      Nothing -> error $ "Local " <> n <> " not found"

  assemble (LLIR.StoreLocal name) = increase $ do
    locals <- readIORef localPool
    case Map.lookup name locals of
      Just address -> do
        address' <- negIdx address
        pure [BC.StoreLocal address']
      Nothing -> error $ "Local " <> name <> " not found"

  assemble (LLIR.LoadConstant address) = increase $ do
    pure [BC.LoadConstant address]

  assemble (LLIR.LoadGlobal address) = increase $ do
    globals <- readIORef globalPool
    case Map.lookup address globals of
      Just address' -> pure [BC.LoadGlobal address']
      Nothing -> error $ "Global " <> address <> " not found"

  assemble (LLIR.StoreGlobal address) = increase $ do
    globals <- readIORef globalPool
    case Map.lookup address globals of
      Just address' -> pure [BC.StoreGlobal address']
      Nothing -> error $ "Global " <> address <> " not found"
  
  assemble (LLIR.LoadNative name) = increase $ do
    pure [BC.LoadNative name]

  assemble LLIR.Update = increase $ do
    pure [BC.Update]
  assemble LLIR.Return = increase $ do
    pure [BC.Return]
  assemble (LLIR.Compare cmp) = increase $ do
    pure [BC.Compare cmp]
  assemble (LLIR.MakeList size) = increase $ do
    pure [BC.MakeList size]
  assemble (LLIR.ListGet index) = increase $ do
    pure [BC.ListGet index]

  assemble (LLIR.Call arity) = increase $ do
    pure [BC.Call arity]
  assemble (LLIR.CallGlobal index arity) = increase $ do
    globals <- readIORef globalPool
    case Map.lookup index globals of
      Just idx -> pure [BC.CallGlobal idx arity]
      Nothing -> error $ "Global " <> index <> " not found"
  assemble (LLIR.CallLocal index arity) = increase $ do
    locals <- readIORef localPool
    case Map.lookup index locals of
      Just idx -> do
        addr <- negIdx idx
        pure [BC.CallLocal addr arity]
      Nothing -> error $ "Local " <> index <> " not found"
  assemble (LLIR.CallNative index arity) = increase $ do
    pure [BC.CallNative index arity]

  assemble (LLIR.JumpIfFalse address) = increase $ do
    pure [BC.JumpIfFalse address]
  assemble (LLIR.JumpRel address) = increase $ do
    pure [BC.JumpRel address]
  assemble LLIR.GetIndex = increase $ do
    pure [BC.GetIndex]
  assemble LLIR.Special = increase $ do
    pure [BC.Special]
  assemble LLIR.Halt = increase $ do
    pure [BC.Halt]

  assemble (LLIR.MakeEvent {}) = compilerError "Anonymous events should not appear in the bytecode"
  assemble (LLIR.Send args body) = increase $ do
    pure [BC.Send args body]
  assemble LLIR.Spawn = increase $ do
    pure [BC.Spawn]
  assemble LLIR.MakeMutable = increase $ do
    pure [BC.MakeMutable]
  assemble (LLIR.Loc a b c) = increase $ do
    pure [BC.Loc a b c]
  assemble LLIR.Add = increase $ do
    pure [BC.Add]
  assemble LLIR.Sub = increase $ do
    pure [BC.Sub]
  assemble LLIR.Mul = increase $ do
    pure [BC.Mul]
  assemble LLIR.Div = increase $ do
    pure [BC.Div]
  assemble LLIR.Mod = increase $ do
    pure [BC.Mod]

  assemble (LLIR.TryCatch jumpAddr) = increase $ do
    pure [BC.TryCatch jumpAddr]

  assemble LLIR.GetValue = increase $ do
    pure [BC.GetValue]

  assemble (LLIR.GetRecordAccess index) = increase $ do
    pure [BC.GetRecordAccess index]

  assemble (LLIR.MakeRecord size) = increase $ do
    pure [BC.MakeRecord size]

  assemble (LLIR.While body instructions) = do
    loopBegin <- readIORef currentIPC
    body' <- assemble body
    
    modifyIORef' currentIPC (+ 1)

    oldContinue <- readIORef currentContinueAddress
    oldBreak <- readIORef currentBreakAddress

    currentIPC' <- readIORef currentIPC
    -- Continue should jump back to the beginning of the loop condition
    writeIORef currentContinueAddress (Just loopBegin)
    -- Break should jump to after the entire while construct
    writeIORef currentBreakAddress (Just (currentIPC' + computeLen instructions + 1))

    instructions' <- assemble instructions

    writeIORef currentContinueAddress oldContinue
    writeIORef currentBreakAddress oldBreak

    modifyIORef' currentIPC (+ 1)

    pure $ body' <> [BC.JumpIfFalse (length instructions' + 2)] <> instructions' <> [BC.JumpRel (- (length instructions' + length body' + 1))]

  assemble (LLIR.Break _) = do
    mbBreak <- readIORef currentBreakAddress
    case mbBreak of
      Just addr -> increase $ pure [BC.Jump addr]
      Nothing -> compilerError "Break statement outside of loop"
    
  assemble (LLIR.Continue _) = do
    mbContinue <- readIORef currentContinueAddress
    case mbContinue of
      Just addr -> increase $ pure [BC.Jump addr]
      Nothing -> compilerError "Continue statement outside of loop"
    
  assemble LLIR.UnLoc = increase $ do
    pure [BC.UnLoc]

computeLen :: [LLIR.Instruction] -> Int
computeLen =
  foldl' (\acc x -> acc + case x of
    LLIR.While cond body -> computeLen cond + computeLen body + 2
    _ -> 1
    ) 0

instance Assemble LLIR.Segment where
  assemble (LLIR.EventOn {}) = 
    compilerError "Events should not appear in the bytecode, they should be converted to functions"

  assemble (LLIR.Function name _ ls freed instructions) = do
    let freed' = Map.fromList freed

    globals <- readIORef globalPool
    case Map.lookup name globals of
      Just addr -> do
        modifyIORef' currentIPC (+ 2)
        instructions' <- withLocals freed' $ assemble instructions
        pure (BC.MakeFunctionAndStore addr (length instructions' + 1) ls : instructions' <> [BC.Return])
      
      Nothing -> error $ "Global " <> name <> " not found"

  assemble (LLIR.Event {}) = 
    compilerError "Events should not appear in the bytecode, they should be converted to functions"

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

  writeIORef currentIPC 0
  writeIORef globalPool globals'

  res <- assemble xs

  pure $ res <> [BC.Halt]