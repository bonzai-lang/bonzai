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