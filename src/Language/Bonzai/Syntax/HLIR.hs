{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Bonzai.Syntax.HLIR
  ( Update (..),
    Expression (..),
    DataConstructor (..),
    Pattern (..),
    pattern MkExprBinary,
    pattern MkExprString,
    pattern MkExprTuple,
    pattern MkExprMutableOperation,
    -- Re-exports
    module Lit,
    module Ann,
    module Pos,
    module Ty,
    -- Type families
    HLIR,
    TLIR,
  )
where

import Data.Text qualified as T
import GHC.Show qualified as S
import GHC.TypeLits (Symbol)
import Language.Bonzai.Syntax.Internal.Annotation as Ann
import Language.Bonzai.Syntax.Internal.Literal as Lit
import Language.Bonzai.Syntax.Internal.Position as Pos
import Language.Bonzai.Syntax.Internal.Type as Ty
import Prelude hiding (Type)

-- | UPDATE TYPE
-- | Update type is used to represent updates to variables, fields, and indices.
-- | It is used to update the value of a variable, a field of a record, or an
-- | element of a list.
data Update f t
  = MkUpdtVariable (Annotation (f t))
  | MkUpdtField (Update f t) Text
  | MkUpdtIndex (Update f t) (Expression f t)
  deriving (Show, Generic)

-- | EXPRESSION TYPE
-- | Expression type is used to represent expressions in Bonzai. It is used to
-- | represent literals, variables, applications, lambdas, ternaries, updates,
-- | let bindings, blocks, actors, on blocks, send blocks, require blocks, locations,
-- | spawns, lists, natives, interfaces, while loops, index expressions, data types,
-- | match expressions, live expressions, unwrap live expressions, wrap live expressions,
-- | and public expressions.
data Expression f t
  = MkExprLiteral Literal
  | MkExprVariable (Annotation (f t))
  | MkExprApplication (Expression f t) [Expression f t]
  | MkExprLambda [Annotation (f t)] (f t) (Expression f t)
  | MkExprTernary (Expression f t) (Expression f t) (Expression f t)
  | MkExprUpdate (Update f t) (Expression f t)
  | MkExprLet (Set Text) (Either (Annotation (f t)) (Pattern f t)) (Expression f t) (Expression f t)
  | MkExprMut (Expression f t)
  | MkExprBlock [Expression f t]
  | MkExprRequire Text (Set Text)
  | MkExprLoc (Expression f t) Position
  | MkExprList [Expression f t]
  | MkExprNative (Annotation [Text]) Ty.Type
  | MkExprInterface (Annotation [QuVar]) [Annotation t]
  | MkExprWhile (Expression f t) (Expression f t)
  | MkExprIndex (Expression f t) (Expression f t)
  | MkExprData (Annotation [Text]) [DataConstructor t]
  | MkExprMatch (Expression f t) [(Pattern f t, Expression f t, Maybe Position)]
  | MkExprPublic (Expression f t)
  | MkExprRecordExtension (Expression f t) Text Bool (Expression f t)
  | MkExprRecordEmpty
  | MkExprRecordAccess (Expression f t) Text
  | MkExprSingleIf (Expression f t) (Expression f t)
  | MkExprBreak
  | MkExprContinue
  | MkExprReturn (Expression f t)
  deriving (Generic)

-- | DATA CONSTRUCTOR TYPE
-- | Data constructor type is used to represent data constructors in Bonzai. It is
-- | used to represent constructors of data types.
data DataConstructor t
  = MkDataVariable Text
  | MkDataConstructor Text [t]
  deriving (Generic)

-- | PATTERN TYPE
-- | Pattern type is used to represent patterns in Bonzai. It is used to represent
-- | patterns in match expressions.
data Pattern f t
  = MkPatVariable Text (f t)
  | MkPatConstructor Text [Pattern f t]
  | MkPatLiteral Literal
  | MkPatWildcard
  | MkPatSpecial Text
  | MkPatLocated (Pattern f t) Position
  | MkPatOr (Pattern f t) (Pattern f t)
  | MkPatCondition (Expression f t) (Pattern f t)
  | MkPatList [Pattern f t] (Maybe (Pattern f t)) (f t)
  deriving (Show, Generic)

instance (ToText t, ToText (f t)) => Show (Expression f t) where
  show = T.unpack . toText

-- | BINARY EXPRESSION PATTERN
-- | A pattern synonym to represent binary expressions in Bonzai.
pattern MkExprBinary :: Text -> Expression Maybe t -> Expression Maybe t -> Expression Maybe t
pattern MkExprBinary op a b = MkExprApplication (MkExprVariable (MkAnnotation op Nothing)) [a, b, MkExprRecordEmpty]

-- |  STRING EXPRESSION PATTERN
--  | A pattern synonym to represent string expressions in Bonzai.
pattern MkExprString :: Text -> Expression f t
pattern MkExprString s = MkExprLiteral (MkLitString s)

-- | TUPLE EXPRESSION PATTERN
-- | A pattern synonym to represent tuple expressions in Bonzai.
pattern MkExprTuple :: Expression Maybe t -> Expression Maybe t -> Expression Maybe t
pattern MkExprTuple a b = MkExprApplication (MkExprVariable (MkAnnotation "Tuple" Nothing)) [a, b, MkExprRecordEmpty]

pattern MkExprMutableOperation :: Text -> Expression Maybe t -> Expression Maybe t -> Expression Maybe t
pattern MkExprMutableOperation op a b = MkExprApplication (MkExprVariable (MkAnnotation op Nothing)) [a, b, MkExprRecordEmpty]

type family HLIR (s :: Symbol) where
  HLIR "update" = Update Maybe Type
  HLIR "expression" = Expression Maybe Type
  HLIR "pattern" = Pattern Maybe Type
  HLIR "data" = DataConstructor Type

type family TLIR (s :: Symbol) where
  TLIR "update" = Update Identity Type
  TLIR "expression" = Expression Identity Type
  TLIR "pattern" = Pattern Identity Type
  TLIR "data" = DataConstructor Type

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
  toText (MkExprLet _ (Left a) e b) = T.concat ["let ", toText a, " = ", toText e, " in ", toText b]
  toText (MkExprLet _ (Right p) e b) = T.concat ["let ", toText p, " = ", toText e, " in ", toText b]
  toText (MkExprBlock es) = "{" <> T.concat [T.intercalate "; " (map toText es)] <> "}"
  toText (MkExprRequire n vars) = T.concat ["require ", n, ": ", T.intercalate ", " (toList vars)]
  toText (MkExprLoc e (start, end)) = "((" <> toText start <> "-" <> toText end <> ") " <> toText e <> ")"
  toText (MkExprList es) = T.concat ["[", T.intercalate ", " (map toText es), "]"]
  toText (MkExprNative ann ty) = T.concat ["native ", toText ann.name, "<", T.intercalate ", " ann.value, "> ", toText ty]
  toText (MkExprMut e) = T.concat ["mut ", toText e]
  toText (MkExprInterface ann as) = T.concat ["interface ", toText ann.name, "<", T.intercalate ", " (map toText as), ">"]
  toText (MkExprWhile c e) = T.concat ["while ", toText c, " { ", toText e, " }"]
  toText (MkExprIndex e e') = T.concat [toText e, "[", toText e', "]"]
  toText (MkExprData ann cs) = T.concat ["data ", toText ann.name, "<", T.intercalate ", " (map toText cs), ">"]
  toText (MkExprMatch e cs) = T.concat ["match ", toText e, " { ", T.intercalate ", " (map (\(c, b, _) -> T.concat [toText c, " => ", toText b]) cs), " }"]
  toText (MkExprPublic e) = T.concat ["pub ", toText e]
  toText (MkExprRecordExtension e f False v) = T.concat [toText e, ".", f, " = ", toText v]
  toText (MkExprRecordExtension e f True v) = T.concat [toText e, "?.", f, " = ", toText v]
  toText MkExprRecordEmpty = "{}"
  toText (MkExprRecordAccess e f) = T.concat [toText e, ".", f]
  toText (MkExprSingleIf c t) = T.concat ["if ", toText c, " then { ", toText t, " }"]
  toText MkExprBreak = "break"
  toText MkExprContinue = "continue"
  toText (MkExprReturn e) = T.concat ["return ", toText e]

instance (ToText t) => ToText (DataConstructor t) where
  toText (MkDataVariable v) = v
  toText (MkDataConstructor c ts) = T.concat [c, "<", T.intercalate ", " (map toText ts), ">"]

instance (ToText (f t), ToText t) => ToText (Pattern f t) where
  toText (MkPatVariable n t) = T.concat [n, ":", toText t]
  toText (MkPatConstructor n ps) = T.concat [n, "(", T.intercalate ", " (map toText ps), ")"]
  toText (MkPatLiteral l) = toText l
  toText MkPatWildcard = "_"
  toText (MkPatSpecial s) = s
  toText (MkPatLocated p _) = toText p
  toText (MkPatOr p p') = T.concat [toText p, " | ", toText p']
  toText (MkPatCondition e p) = T.concat [toText p, " if ", toText e]
  toText (MkPatList ps Nothing _) = T.concat ["[", T.intercalate ", " (map toText ps), "]"]
  toText (MkPatList ps (Just p) _) = T.concat ["[", T.intercalate ", " (map toText ps), " ..", toText p, "]"]

instance (ToText t, ToText (f t)) => ToText [Expression f t] where
  toText = T.intercalate "\n" . map toText

instance Locate (Expression f t) where
  locate = MkExprLoc

instance Locate (Pattern f t) where
  locate = MkPatLocated

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
  MkExprLet g a e b == MkExprLet g' a' e' b' = a == a' && e == e' && g == g' && b == b'
  MkExprMut e == MkExprMut e' = e == e'
  MkExprBlock es == MkExprBlock es' = es == es'
  MkExprRequire n v == MkExprRequire n' v' = n == n' && v == v'
  MkExprLoc e _ == MkExprLoc e' _ = e == e'
  MkExprLoc e _ == e' = e == e'
  e == MkExprLoc e' _ = e == e'
  MkExprList es == MkExprList es' = es == es'
  MkExprNative ann ty == MkExprNative ann' ty' = ann == ann' && ty == ty'
  MkExprInterface ann as == MkExprInterface ann' as' = ann == ann' && as == as'
  MkExprWhile c e == MkExprWhile c' e' = c == c' && e == e'
  MkExprIndex e e' == MkExprIndex e'' e''' = e == e'' && e' == e'''
  MkExprData ann cs == MkExprData ann' cs' = ann == ann' && cs == cs'
  MkExprMatch e cs == MkExprMatch e' cs' = e == e' && cs == cs'
  MkExprPublic e == MkExprPublic e' = e == e'
  _ == _ = False

instance (Eq t) => Eq (DataConstructor t) where
  MkDataVariable v == MkDataVariable v' = v == v'
  MkDataConstructor c ts == MkDataConstructor c' ts' = c == c' && ts == ts'
  _ == _ = False

instance (Eq (f t), Eq t) => Eq (Pattern f t) where
  MkPatVariable n t == MkPatVariable n' t' = n == n' && t == t'
  MkPatConstructor n ps == MkPatConstructor n' ps' = n == n' && ps == ps'
  MkPatLiteral l == MkPatLiteral l' = l == l'
  MkPatWildcard == MkPatWildcard = True
  MkPatSpecial s == MkPatSpecial s' = s == s'
  MkPatLocated p _ == MkPatLocated p' _ = p == p'
  MkPatOr p p' == MkPatOr p'' p''' = p == p'' && p' == p'''
  MkPatCondition e p == MkPatCondition e' p' = e == e' && p == p'
  _ == _ = False
