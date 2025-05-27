module Language.Bonzai.Syntax.Internal.Annotation where
import qualified Data.Text as T
import Data.Aeson (ToJSON, FromJSON)

-- | ANNOTATION TYPE
-- | Annotations are used to attach metadata to AST nodes. For instance,
-- | annotations can be used to attach types to variables, or to attach
-- | quantified types to names in a type signature.
data Annotation a = MkAnnotation {
  name :: Text,
  value :: a
} deriving (Ord, Show, Generic)

instance {-# OVERLAPS #-} ToText a => ToText (Annotation (Maybe a)) where
  toText (MkAnnotation n (Just v)) = T.concat [n, ": ", toText v]
  toText (MkAnnotation n Nothing) = n

instance Eq (Annotation a) where
  (MkAnnotation n1 _) == (MkAnnotation n2 _) = n1 == n2

instance ToText a => ToText (Annotation a) where
  toText (MkAnnotation n v) = T.concat [n, ": ", toText v]

instance ToJSON a => ToJSON (Annotation a)

instance FromJSON a => FromJSON (Annotation a)

-- | UNANNOTATE
-- | Extract the name and value from an annotation. Used to decompose an annotation
-- | when needed for instance in typed closure conversion.
unannotate :: Annotation a -> (Text, a)
unannotate (MkAnnotation n v) = (n, v)

instance Functor Annotation where
  fmap f (MkAnnotation n v) = MkAnnotation n (f v)

-- | SOME INSTANCES
-- | Defining some typeclass instances for the Annotation type in order to make it
-- | easier to work with and to compute over.

instance Foldable Annotation where
  foldMap f (MkAnnotation _ v) = f v

instance Traversable Annotation where
  traverse f (MkAnnotation n v) = MkAnnotation n <$> f v

instance Monad Annotation where
  MkAnnotation _ v >>= f = case f v of
    MkAnnotation n' v' -> MkAnnotation n' v'

instance Applicative Annotation where
  pure = MkAnnotation mempty
  
  -- We discard the name of the first annotation and keep the name of the second
  -- annotation, because the name of the first annotation is not relevant. Only 
  -- the value is.
  MkAnnotation _ f <*> MkAnnotation n' v = MkAnnotation n' (f v)