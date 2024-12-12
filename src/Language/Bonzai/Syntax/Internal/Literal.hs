module Language.Bonzai.Syntax.Internal.Literal where

import qualified Data.Text as T
import Data.Aeson (ToJSON, FromJSON)

-- | LITERAL TYPE
-- | Literals are the most basic form of data in a programming language. They
-- | are also called atomic or primitive values. Literals are used to represent
-- | numbers, strings, characters, booleans, and other basic types.
-- | 
-- | - Int: An integer literal of the form 0, 1, 2, 3, etc.
-- | - Float: A floating-point literal of the form 0.0, 1.0, 2.0, 3.0, etc.
-- | - Char: A character literal of the form 'a', 'b', 'c', etc.
-- | - String: A string literal of the form "hello", "world", "bonzai", etc.
-- | - Bool: A boolean literal of the form true or false.
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