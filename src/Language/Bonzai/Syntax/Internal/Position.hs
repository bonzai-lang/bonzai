{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Bonzai.Syntax.Internal.Position where
import qualified GHC.IO as IO
import Text.Megaparsec (SourcePos, Pos, initialPos)
import Data.Aeson (ToJSON, FromJSON)

type Position = (SourcePos, SourcePos)

class Locate a where
  locate :: a -> Position -> a

instance Locate a => Locate [a] where
  locate xs p = map (`locate` p) xs

instance Locate a => Locate (Maybe a) where
  locate (Just x) p = Just (locate x p)
  locate Nothing _ = Nothing

instance ToJSON SourcePos

instance ToJSON Pos

instance FromJSON SourcePos

instance FromJSON Pos

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

peekPosition :: MonadIO m => m Position
peekPosition = readIORef positionStack >>= \case
  [] -> error "peekPosition: empty stack"
  x : _ -> pure x

peekPosition' :: MonadIO m => m Position
peekPosition' = readIORef positionStack >>= \case
  [] -> pure (initialPos "", initialPos "")
  x : _ -> pure x
