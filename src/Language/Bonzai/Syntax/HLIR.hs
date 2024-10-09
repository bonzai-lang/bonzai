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
import GHC.Show qualified as S

data Update f t
  = MkUpdtVariable (Annotation (f t))
  | MkUpdtField (Update f t) Text
  | MkUpdtIndex (Update f t) (Expression f t)
  deriving Show

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
  | MkExprActor Text [Expression f t]
  | MkExprOn Text [Annotation (f t)] (Expression f t)
  | MkExprSend (Expression f t) Text [Expression f t]
  | MkExprRequire Text
  | MkExprLoc (Expression f t) Position
  | MkExprSpawn (Expression f t)
  | MkExprList [Expression f t]
  | MkExprNative (Annotation [Text]) Ty.Type
  | MkExprInterface (Annotation [QuVar]) [Annotation t]
  | MkExprWhile (Expression f t) (Expression f t)
  | MkExprIndex (Expression f t) (Expression f t)

instance (ToText t, ToText (f t)) => Show (Expression f t) where
  show = T.unpack . toText

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
  toText (MkExprActor i e) = T.concat ["event ",i , " ", toText e]
  toText (MkExprOn n as e) = T.concat ["on ", n, "(", T.intercalate ", " (map toText as), ") { ", toText e, " }"]
  toText (MkExprSend e n e') = T.concat ["(", toText e, ") -> ", n, "(", toText e', ")"]
  toText (MkExprRequire n) = T.concat ["require ", n]
  toText (MkExprLoc e _) = toText e
  toText (MkExprSpawn e) = T.concat ["spawn ", toText e]
  toText (MkExprList es) = T.concat ["[", T.intercalate ", " (map toText es), "]"]
  toText (MkExprNative ann ty) = T.concat ["native ", toText ann.name, "<", T.intercalate ", " ann.value, "> ", toText ty]
  toText (MkExprMut a e) = T.concat ["mut ", toText a, " = ", toText e]
  toText (MkExprInterface ann as) = T.concat ["interface ", toText ann.name, "<", T.intercalate ", " (map toText as), ">"]
  toText (MkExprWhile c e) = T.concat ["while ", toText c, " { ", toText e, " }"]
  toText (MkExprIndex e e') = T.concat [toText e, "[", toText e', "]"]

instance (ToText t, ToText (f t)) => ToText [Expression f t] where
  toText = T.intercalate "\n" . map toText

instance Locate (Expression f t) where
  locate = MkExprLoc

instance (Eq (f t), Eq t) => Eq (Update f t) where
  MkUpdtVariable a == MkUpdtVariable b = a == b
  MkUpdtField u f == MkUpdtField u' f' = u == u' && f == f'
  MkUpdtIndex u e == MkUpdtIndex u' e' = u == u' && e == e'
  _ == _ = False

instance (Eq (f t), Eq t) => Eq (Expression f t) where
  MkExprLiteral l == MkExprLiteral l' = l == l'
  MkExprVariable a == MkExprVariable b = a == b
  MkExprApplication e es == MkExprApplication e' es' = e == e' && es == es'
  MkExprLambda as ret e == MkExprLambda as' ret' e' = as == as' && ret == ret' && e == e'
  MkExprTernary c t e == MkExprTernary c' t' e' = c == c' && t == t' && e == e'
  MkExprUpdate u e == MkExprUpdate u' e' = u == u' && e == e'
  MkExprLet a e == MkExprLet a' e' = a == a' && e == e'
  MkExprMut a e == MkExprMut a' e' = a == a' && e == e'
  MkExprBlock es == MkExprBlock es' = es == es'
  MkExprActor i e == MkExprActor i' e' = i == i' && e == e'
  MkExprOn n as e == MkExprOn n' as' e' = n == n' && as == as' && e == e'
  MkExprSend e n es == MkExprSend e' n' es' = e == e' && n == n' && es == es'
  MkExprRequire n == MkExprRequire n' = n == n'
  MkExprLoc e _ == MkExprLoc e' _ = e == e'
  MkExprLoc e _ == e' = e == e'
  e == MkExprLoc e' _ = e == e'
  MkExprSpawn e == MkExprSpawn e' = e == e'
  MkExprList es == MkExprList es' = es == es'
  MkExprNative ann ty == MkExprNative ann' ty' = ann == ann' && ty == ty'
  MkExprInterface ann as == MkExprInterface ann' as' = ann == ann' && as == as'
  MkExprWhile c e == MkExprWhile c' e' = c == c' && e == e'
  _ == _ = False