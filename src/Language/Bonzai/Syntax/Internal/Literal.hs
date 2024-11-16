module Language.Bonzai.Syntax.Internal.Literal where

import qualified Data.Text as T
import Data.Aeson (ToJSON, FromJSON)

data Literal
  = MkLitInt Integer
  | MkLitFloat Double
  | MkLitChar Char
  | MkLitString Text
  | MkLitBool Bool
  deriving (Eq, Ord, Show, Generic)

instance ToText Literal where
  toText (MkLitInt i) = T.pack (show i)
  toText (MkLitFloat f) = T.pack (show f)
  toText (MkLitChar c) = T.pack (show c)
  toText (MkLitString s) = T.pack (show s)
  toText (MkLitBool b) = T.pack (show b)

instance ToJSON Literal

instance FromJSON Literal