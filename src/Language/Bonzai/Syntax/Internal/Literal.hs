module Language.Bonzai.Syntax.Internal.Literal where

import qualified Data.Text as T

data Literal
  = MkLitInt Integer
  | MkLitFloat Double
  | MkLitChar Char
  | MkLitString Text
  deriving (Eq, Ord, Show)

instance ToText Literal where
  toText (MkLitInt i) = T.pack (show i)
  toText (MkLitFloat f) = T.pack (show f)
  toText (MkLitChar c) = T.pack (show c)
  toText (MkLitString s) = T.pack (show s)