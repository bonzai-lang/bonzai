{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.Bonzai.Syntax.MLIR (
  Update(..),
  Expression(..),

  pattern MkExprFunction,

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
import GHC.Show qualified as S
import qualified Data.Map as Map

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
  | MkExprList [Expression]
  | MkExprNative (Annotation [Text]) Ty.Type
  | MkExprIndex Expression Expression
  | MkExprUnpack Text Expression Expression
  | MkExprMut Expression
  | MkExprLoc Position Expression
  | MkExprWhile Expression Expression
  | MkExprSpecial
  | MkExprRecordAccess Expression Text
  | MkExprRecord (Map Text Expression)
  | MkExprSingleIf Expression Expression
  | MkExprReturn Expression
  | MkExprBreak
  | MkExprContinue

pattern MkExprFunction :: Text -> [Text] -> Expression -> Expression
pattern MkExprFunction f args b = MkExprLet f (MkExprLambda args b)

instance Eq Expression where
  (MkExprVariable a) == (MkExprVariable b) = a == b
  (MkExprLiteral l1) == (MkExprLiteral l2) = l1 == l2
  (MkExprApplication e1 es1) == (MkExprApplication e2 es2) = e1 == e2 && es1 == es2
  (MkExprLambda as1 e1) == (MkExprLambda as2 e2) = as1 == as2 && e1 == e2
  (MkExprTernary c1 t1 e1) == (MkExprTernary c2 t2 e2) = c1 == c2 && t1 == t2 && e1 == e2
  (MkExprBinary op1 e11 e12) == (MkExprBinary op2 e21 e22) = op1 == op2 && e11 == e21 && e12 == e22
  (MkExprUpdate u1 e1) == (MkExprUpdate u2 e2) = u1 == u2 && e1 == e2
  (MkExprLet a1 e1) == (MkExprLet a2 e2) = a1 == a2 && e1 == e2
  (MkExprBlock es1) == (MkExprBlock es2) = es1 == es2
  (MkExprList es1) == (MkExprList es2) = es1 == es2
  (MkExprNative n1 ty1) == (MkExprNative n2 ty2) = n1.name == n2.name && n1.value == n2.value && ty1 == ty2
  (MkExprIndex e11 i11) == (MkExprIndex e21 i21) = e11 == e21 && i11 == i21
  (MkExprUnpack a11 e11 b11) == (MkExprUnpack a21 e21 b21) = a11 == a21 && e11 == e21 && b11 == b21
  (MkExprMut m1) == (MkExprMut m2) = m1 == m2
  (MkExprLoc _ m1) == (MkExprLoc _ m2) = m1 == m2
  (MkExprWhile c1 e1) == (MkExprWhile c2 e2) = c1 == c2 && e1 == e2
  MkExprSpecial == MkExprSpecial = True
  (MkExprRecordAccess e1 f1) == (MkExprRecordAccess e2 f2) = e1 == e2 && f1 == f2
  (MkExprRecord m1) == (MkExprRecord m2) = Map.toList m1 == Map.toList m2
  (MkExprSingleIf c1 e1) == (MkExprSingleIf c2 e2) = c1 == c2 && e1 == e2
  (MkExprReturn e1) == (MkExprReturn e2) = e1 == e2
  MkExprBreak == MkExprBreak = True
  MkExprContinue == MkExprContinue = True
  _ == _ = False

instance Show Expression where
  show = T.unpack . toText

instance S.Show Update where
  show = T.unpack . toText

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
  toText (MkExprList es) = T.concat ["[", T.intercalate ", " (map toText es), "]"]
  toText (MkExprNative ann ty) = T.concat ["native ", toText ann.name, "<", T.intercalate ", " ann.value, "> ", toText ty]
  toText (MkExprIndex e e') = T.concat [toText e, "[", toText e', "]"]
  toText (MkExprUnpack a e e') = T.concat ["let ", a, " = ", toText e, " in ", toText e']
  toText (MkExprMut e) = T.concat ["mut ", toText e]
  toText (MkExprLoc _ e) = toText e
  toText (MkExprWhile c e) = T.concat ["while ", toText c, " { ", toText e, " }"]
  toText MkExprSpecial = "<special>"
  toText (MkExprBinary op e1 e2) = T.concat ["(", toText e1, " ", toText op, " ", toText e2, ")"]
  toText (MkExprRecordAccess e f) = T.concat [toText e, ".", f]
  toText (MkExprSingleIf c e) = T.concat ["if ", toText c, " { ", toText e, " }"]
  toText (MkExprReturn e) = T.concat ["return ", toText e]  
  toText (MkExprRecord m) = T.concat ["{", T.intercalate ", " (map (\(k, v) -> k <> ": " <> toText v) (Map.toList m)), "}"]
  toText MkExprBreak = "break"
  toText MkExprContinue = "continue"

instance ToText [Expression] where
  toText = T.intercalate "\n" . map toText
