module Language.Bonzai.Syntax.Internal.Annotation where
import qualified Data.Text as T
import Data.Aeson (ToJSON, FromJSON)

data Annotation a = MkAnnotation {
  name :: Text,
  value :: a
} deriving (Eq, Ord, Show, Generic)

instance {-# OVERLAPS #-} ToText a => ToText (Annotation (Maybe a)) where
  toText (MkAnnotation n (Just v)) = T.concat [n, ": ", toText v]
  toText (MkAnnotation n Nothing) = n

instance ToText a => ToText (Annotation a) where
  toText (MkAnnotation n v) = T.concat [n, ": ", toText v]

instance ToJSON a => ToJSON (Annotation a)

instance FromJSON a => FromJSON (Annotation a)

unannotate :: Annotation a -> (Text, a)
unannotate (MkAnnotation n v) = (n, v)

instance Functor Annotation where
  fmap f (MkAnnotation n v) = MkAnnotation n (f v)

instance Foldable Annotation where
  foldMap f (MkAnnotation _ v) = f v

instance Traversable Annotation where
  traverse f (MkAnnotation n v) = MkAnnotation n <$> f v

instance Monad Annotation where
  MkAnnotation _ v >>= f = case f v of
    MkAnnotation n' v' -> MkAnnotation n' v'

instance Applicative Annotation where
  pure = MkAnnotation mempty
  MkAnnotation n f <*> MkAnnotation n' v = MkAnnotation (n <> n') (f v)