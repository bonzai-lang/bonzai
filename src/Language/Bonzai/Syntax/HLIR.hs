{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.Bonzai.Syntax.HLIR (
  Update(..),
  Expression(..),

  pattern MkExprBinary,

  -- Re-exports
  module Lit,
  module Ann,
  module Pos,
  module Ty,

  -- Type families
  HLIR,
  TLIR,
) where
import Language.Bonzai.Syntax.Internal.Literal as Lit
import Language.Bonzai.Syntax.Internal.Annotation as Ann
import Language.Bonzai.Syntax.Internal.Position as Pos
import Language.Bonzai.Syntax.Internal.Type as Ty
import qualified Data.Text as T
import GHC.TypeLits (Symbol)
import Prelude hiding (Type)

data Update f t 
  = MkUpdtVariable (Annotation (f t))
  | MkUpdtField (Update f t) Text
  | MkUpdtIndex (Update f t) (Expression f t)
  deriving (Eq)

data Expression f t
  = MkExprLiteral Literal
  | MkExprVariable (Annotation (f t))
  | MkExprApplication (Expression f t) [Expression f t]
  | MkExprLambda [Annotation (f t)] (f t) (Expression f t)
  | MkExprTernary (Expression f t) (Expression f t) (Expression f t)
  | MkExprUpdate (Update f t) (Expression f t)
  | MkExprLet (Annotation (f t)) (Expression f t)
  | MkExprMut (Annotation (f t)) (Expression f t)
  | MkExprBlock [Expression f t]
  | MkExprModule Text [Expression f t]
  | MkExprEvent [Expression f t]
  | MkExprOn Text [Annotation (f t)] (Expression f t)
  | MkExprSend (Expression f t) Text [Expression f t]
  | MkExprRequire Text
  | MkExprLoc (Expression f t) Position
  | MkExprSpawn (Expression f t)
  | MkExprList [Expression f t]
  | MkExprNative (Annotation [Text]) Ty.Type
  deriving (Eq)

pattern MkExprBinary :: Text -> f t -> Expression f t -> Expression f t -> Expression f t
pattern MkExprBinary op t a b = MkExprApplication (MkExprVariable (MkAnnotation op t)) [a, b]

type family HLIR (s :: Symbol) where
  HLIR "update" = Update Maybe Type
  HLIR "expression" = Expression Maybe Type

type family TLIR (s :: Symbol) where
  TLIR "update" = Update Identity Type
  TLIR "expression" = Expression Identity Type

instance (ToText t, ToText (f t)) => ToText (Update f t) where
  toText (MkUpdtVariable a) = toText a
  toText (MkUpdtField u f) = T.concat [toText u, ".", f]
  toText (MkUpdtIndex u e) = T.concat [toText u, "[", toText e, "]"]

instance (ToText t, ToText (f t)) => ToText (Expression f t) where
  toText (MkExprLiteral l) = toText l
  toText (MkExprVariable a) = toText a
  toText (MkExprApplication e es) = T.concat ["(", toText e, ")(", T.intercalate ", " (map toText es), ")"]
  toText (MkExprLambda as ret e) = T.concat ["(", T.intercalate ", " (map toText as), "): ", toText ret, " => ", toText e]
  toText (MkExprTernary c t e) = T.concat [toText c, " ? ", toText t, " : ", toText e]
  toText (MkExprUpdate u e) = T.concat [toText u, " = ", toText e]
  toText (MkExprLet a e) = T.concat ["let ", toText a, " = ", toText e]
  toText (MkExprBlock es) = T.concat [T.intercalate "; " (map toText es)]
  toText (MkExprModule n e) = T.concat ["module ", n, " { ", toText e, " }"]
  toText (MkExprEvent e) = T.concat ["event ", toText e]
  toText (MkExprOn n as e) = T.concat ["on ", n, "(", T.intercalate ", " (map toText as), ") { ", toText e, " }"]
  toText (MkExprSend e n e') = T.concat ["(", toText e, ") -> ", n, "(", toText e', ")"]
  toText (MkExprRequire n) = T.concat ["require ", n]
  toText (MkExprLoc e _) = toText e
  toText (MkExprSpawn e) = T.concat ["spawn ", toText e]
  toText (MkExprList es) = T.concat ["[", T.intercalate ", " (map toText es), "]"]
  toText (MkExprNative ann ty) = T.concat ["native ", toText ann.name, "<", T.intercalate ", " ann.value, "> ", toText ty]
  toText (MkExprMut a e) = T.concat ["mut ", toText a, " = ", toText e]

instance (ToText t, ToText (f t)) => ToText [Expression f t] where
  toText = T.intercalate "\n" . map toText

instance Locate (Expression f t) where
  locate = MkExprLoc
