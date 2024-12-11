{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Bonzai.Syntax.TMLIR (
  Update(..),
  Expression(..),
  DataConstructor(..),

  -- Re-exports
  module Lit,
  module Ann,
  module Pos,
  module Ty,

  -- Type families
  TMLIR,
) where

import Language.Bonzai.Syntax.Internal.Literal as Lit
import Language.Bonzai.Syntax.Internal.Annotation as Ann
import Language.Bonzai.Syntax.Internal.Position as Pos
import Language.Bonzai.Syntax.Internal.Type as Ty
import qualified Data.Text as T
import GHC.TypeLits (Symbol)

data Update
  = MkUpdtVariable Text Ty.Type
  | MkUpdtField Update Text
  | MkUpdtUnref Update
  | MkUpdtIndex Update Expression
  deriving (Eq, Show)

data Expression
  = MkExprLiteral Literal
  | MkExprVariable Text Ty.Type
  | MkExprApplication Expression [Expression] Ty.Type
  | MkExprLambda [Ann.Annotation Ty.Type] Ty.Type Expression
  | MkExprTernary Expression Expression Expression Ty.Type
  | MkExprField Expression Text
  | MkExprUpdate Update Expression
  | MkExprLet (Set Text) Text Ty.Type Expression
  | MkExprBlock [Expression] Ty.Type
  | MkExprActor Ty.Type [Expression]
  | MkExprOn Text [Ann.Annotation Ty.Type] Expression
  | MkExprSend Expression Text [Expression] Ty.Type
  | MkExprSpawn Expression
  | MkExprList [Expression]
  | MkExprNative (Annotation [Text]) Ty.Type
  | MkExprIndex Expression Expression
  | MkExprUnpack Text Ty.Type Expression Expression
  | MkExprWhile Expression Expression
  | MkExprInterface (Ann.Annotation [QuVar]) [Ann.Annotation Ty.Type]
  | MkExprData (Ann.Annotation [QuVar]) [DataConstructor]
  | MkExprStruct Text [(Maybe Text, Expression)]
  | MkExprRef Expression Ty.Type
  | MkExprUnref Expression
  | MkExprCast Expression Ty.Type
  | MkExprSizeOf Ty.Type
  deriving (Eq, Show)

data DataConstructor
  = MkDataVariable Text
  | MkDataConstructor Text [Ty.Type]
  deriving (Eq, Show)

type family TMLIR (s :: Symbol) where
  TMLIR "update" = Update
  TMLIR "expression" = Expression

instance ToText Update where
  toText (MkUpdtVariable a _) = toText a
  toText (MkUpdtField u f) = T.concat [toText u, ".", f]
  toText (MkUpdtIndex u e) = T.concat [toText u, "[", toText e, "]"]
  toText (MkUpdtUnref u) = T.concat ["*", toText u]

instance ToText Expression where
  toText (MkExprLiteral l) = toText l
  toText (MkExprVariable a _) = toText a
  toText (MkExprApplication e es _) = T.concat [toText e, "(", T.intercalate ", " (map toText es), ")"]
  toText (MkExprLambda as _ e) = T.concat ["(", T.intercalate ", " (map toText as), ") => ", toText e]
  toText (MkExprTernary c t e _) = T.concat [toText c, " ? ", toText t, " : ", toText e]
  toText (MkExprUpdate u e) = T.concat [toText u, " = ", toText e]
  toText (MkExprLet _ a t e) = T.concat ["let ", toText a, ": ", toText t, " = ", toText e]
  toText (MkExprBlock es _) = T.concat ["{", T.intercalate "; " (map toText es), "}"]
  toText (MkExprActor _ e) = T.concat ["event ", toText e]
  toText (MkExprOn n as e) = T.concat ["on ", n, "(", T.intercalate ", " (map toText as), ") { ", toText e, " }"]
  toText (MkExprSend e n e' _) = T.concat ["(", toText e, ") -> ", n, "(", T.intercalate ", " (map toText e'), ")"]
  toText (MkExprSpawn e) = T.concat ["spawn ", toText e]
  toText (MkExprList es) = T.concat ["[", T.intercalate ", " (map toText es), "]"]
  toText (MkExprNative ann ty) = T.concat ["native ", toText ann.name, "<", T.intercalate ", " ann.value, "> ", toText ty]
  toText (MkExprIndex e e') = T.concat [toText e, "[", toText e', "]"]
  toText (MkExprUnpack a _ e e') = T.concat ["let ", a, " = ", toText e, " in ", toText e']
  toText (MkExprWhile c e) = T.concat ["while ", toText c, " { ", toText e, " }"]
  toText (MkExprField e f) = T.concat [toText e, ".", f]
  toText (MkExprInterface ann anns) = T.concat ["interface ", toText ann.name, "<", T.intercalate ", " ann.value, "> { ", T.intercalate ", " (map toText anns), " }"]
  toText (MkExprData ann _) = T.concat ["data ", toText ann.name, "<", T.intercalate ", " ann.value, ">", " { ... }"]
  toText (MkExprStruct n anns) = T.concat [n, " { ", T.intercalate ", " (map (\(n', v) -> toText n' <> ": " <> toText v) anns), " }"]
  toText (MkExprRef e _) = T.concat ["ref ", toText e]
  toText (MkExprUnref e) = T.concat ["unref ", toText e]
  toText (MkExprCast e t) = T.concat ["(", toText t, ") ", toText e]
  toText (MkExprSizeOf t) = T.concat ["sizeof(", toText t, ")"]

instance ToText a => ToText (Maybe a) where
  toText (Just a) = toText a
  toText Nothing = ""

instance ToText DataConstructor where
  toText (MkDataVariable a) = toText a
  toText (MkDataConstructor a as) = T.concat [a, "<", T.intercalate ", " (map toText as), ">"]

instance ToText [Expression] where
  toText = T.intercalate "\n" . map toText
