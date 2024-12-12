{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Bonzai.Syntax.Internal.Position where
import qualified GHC.IO as IO
import Text.Megaparsec (SourcePos, Pos, initialPos)
import Data.Aeson (ToJSON, FromJSON)

-- | POSITION TYPE
-- | Positions are used to represent the location of a token in a source file.
-- | Positions consist of a tuple of two SourcePos values, which represent the
-- | start and end positions of a token.
-- | So a position basically holds the start and end positions of a token in a 
-- | source file.
type Position = (SourcePos, SourcePos)

-- | LOCATE TYPECLASS
-- | Locate typeclass is used to attach a position to an arbitrary AST node.
-- | It allows the compiler to use a single function to attach positions to different
-- | data types.
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

-- | POSITION STACK
-- | The position stack is used to keep track of the current position in the source
-- | file. For instance, it is used within typechecking pass to keep track of the 
-- | positions and then attach them to errors.
{-# NOINLINE positionStack #-}
positionStack :: IORef [Position]
positionStack = IO.unsafePerformIO $ newIORef []

-- | PUSH POSITION
-- | Push a new position onto the position stack.
pushPosition :: MonadIO m => Position -> m ()
pushPosition p = modifyIORef positionStack (p :)

-- | POP POSITION
-- | Pop a position from the position stack.
-- | It throws an error if the stack is empty.
popPosition :: MonadIO m => m Position
popPosition = atomicModifyIORef positionStack $ \case
  [] -> error "popPosition: empty stack"
  x : xs -> (xs, x)

-- | POP POSITION
-- | Pop a position from the position stack.
-- | It returns the initial position if the stack is empty.
popPosition' :: MonadIO m => m Position
popPosition' = atomicModifyIORef positionStack $ \case
  [] -> ([], (initialPos "", initialPos ""))
  x : xs -> (xs, x)

-- | PEEK POSITION
-- | Peek at the current position on the position stack.
-- | It throws an error if the stack is empty.
-- | Peeking is basically looking at the top of the stack without removing it.
peekPosition :: MonadIO m => m Position
peekPosition = readIORef positionStack >>= \case
  [] -> error "peekPosition: empty stack"
  x : _ -> pure x

-- | PEEK POSITION
-- | Peek at the current position on the position stack.
-- | It returns the initial position if the stack is empty.
peekPosition' :: MonadIO m => m Position
peekPosition' = readIORef positionStack >>= \case
  [] -> pure (initialPos "", initialPos "")
  x : _ -> pure x
