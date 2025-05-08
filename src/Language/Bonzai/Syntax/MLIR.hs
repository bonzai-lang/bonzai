{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.Bonzai.Syntax.MLIR (
  Update(..),
  Expression(..),

  pattern MkExprFunction,
  pattern MkExprEventDef,

  -- Re-exports
  module Lit,
  module Ann,
  module Pos,
  module Ty,

  -- Type families
  MLIR,
) where

import Language.Bonzai.Syntax.Internal.Literal as Lit
import Language.Bonzai.Syntax.Internal.Annotation as Ann
import Language.Bonzai.Syntax.Internal.Position as Pos
import Language.Bonzai.Syntax.Internal.Type as Ty
import qualified Data.Text as T
import GHC.TypeLits (Symbol)

-- | UPDATE TYPE
-- | Untyped representation of an update operation in Bonzai.
-- | An update operation is used to update a variable, a field, or an index.
data Update
  = MkUpdtVariable Text
  | MkUpdtField Update Text
  | MkUpdtIndex Update Expression
  deriving (Eq)

-- | EXPRESSION TYPE
-- | Untyped representation of an expression in Bonzai.
-- | An expression is used to represent a computation in Bonzai.
data Expression
  = MkExprLiteral Literal
  | MkExprVariable Text
  | MkExprApplication Expression [Expression]
  | MkExprLambda [Text] Expression
  | MkExprTernary Expression Expression Expression
  | MkExprBinary Text Expression Expression
  | MkExprUpdate Update Expression
  | MkExprLet Text Expression
  | MkExprBlock [Expression]
  | MkExprEvent [Expression]
  | MkExprOn Text [Text] Expression
  | MkExprSend Expression Text [Expression]
  | MkExprSpawn Expression
  | MkExprList [Expression]
  | MkExprNative (Annotation [Text]) Ty.Type
  | MkExprIndex Expression Expression
  | MkExprUnpack Text Expression Expression
  | MkExprMut Expression
  | MkExprLoc Position Expression
  | MkExprWhile Expression Expression
  | MkExprSpecial
  deriving (Eq)

pattern MkExprFunction :: Text -> [Text] -> Expression -> Expression
pattern MkExprFunction f args b = MkExprLet f (MkExprLambda args b)

pattern MkExprEventDef :: Text -> [Expression] -> Expression
pattern MkExprEventDef n es = MkExprLet n (MkExprEvent es)

type family MLIR (s :: Symbol) where
  MLIR "update" = Update
  MLIR "expression" = Expression

instance ToText Update where
  toText (MkUpdtVariable a) = toText a
  toText (MkUpdtField u f) = T.concat [toText u, ".", f]
  toText (MkUpdtIndex u e) = T.concat [toText u, "[", toText e, "]"]

instance ToText Expression where
  toText (MkExprLiteral l) = toText l
  toText (MkExprVariable a) = toText a
  toText (MkExprApplication e es) = T.concat [toText e, "(", T.intercalate ", " (map toText es), ")"]
  toText (MkExprLambda as e) = T.concat ["(", T.intercalate ", " (map toText as), ") => ", toText e]
  toText (MkExprTernary c t e) = T.concat [toText c, " ? ", toText t, " : ", toText e]
  toText (MkExprUpdate u e) = T.concat [toText u, " = ", toText e]
  toText (MkExprLet a e) = T.concat ["let ", toText a, " = ", toText e]
  toText (MkExprBlock es) = T.concat ["{", T.intercalate "; " (map toText es), "}"]
  toText (MkExprEvent e) = T.concat ["event ", toText e]
  toText (MkExprOn n as e) = T.concat ["on ", n, "(", T.intercalate ", " (map toText as), ") { ", toText e, " }"]
  toText (MkExprSend e n e') = T.concat ["(", toText e, ") -> ", n, "(", T.intercalate ", " (map toText e'), ")"]
  toText (MkExprSpawn e) = T.concat ["spawn ", toText e]
  toText (MkExprList es) = T.concat ["[", T.intercalate ", " (map toText es), "]"]
  toText (MkExprNative ann ty) = T.concat ["native ", toText ann.name, "<", T.intercalate ", " ann.value, "> ", toText ty]
  toText (MkExprIndex e e') = T.concat [toText e, "[", toText e', "]"]
  toText (MkExprUnpack a e e') = T.concat ["let ", a, " = ", toText e, " in ", toText e']
  toText (MkExprMut e) = T.concat ["mut ", toText e]
  toText (MkExprLoc _ e) = toText e
  toText (MkExprWhile c e) = T.concat ["while ", toText c, " { ", toText e, " }"]
  toText MkExprSpecial = "<special>"
  toText (MkExprBinary op e1 e2) = T.concat [toText e1, " ", toText op, " ", toText e2]

instance ToText [Expression] where
  toText = T.intercalate "\n" . map toText
