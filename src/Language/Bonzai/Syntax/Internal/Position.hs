{-# LANGUAGE LambdaCase #-}
module Language.Bonzai.Syntax.Internal.Position where
import Text.Megaparsec (SourcePos, initialPos)
import qualified GHC.IO as IO

type Position = (SourcePos, SourcePos)

class Locate a where
  locate :: a -> Position -> a

instance Locate a => Locate [a] where
  locate xs p = map (`locate` p) xs

instance Locate a => Locate (Maybe a) where
  locate (Just x) p = Just (locate x p)
  locate Nothing _ = Nothing

{-# NOINLINE positionStack #-}
positionStack :: IORef [Position]
positionStack = IO.unsafePerformIO $ newIORef []

pushPosition :: MonadIO m => Position -> m ()
pushPosition p = modifyIORef positionStack (p :)

popPosition :: MonadIO m => m Position
popPosition = atomicModifyIORef positionStack $ \case
  [] -> error "popPosition: empty stack"
  x : xs -> (xs, x)

popPosition' :: MonadIO m => m Position
popPosition' = atomicModifyIORef positionStack $ \case
  [] -> ([], (initialPos "", initialPos ""))
  x : xs -> (xs, x)
