{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingVia #-}
module Language.Bonzai.Syntax.HLIR (
  Update(..),
  Expression(..),
  DataConstructor(..),
  Pattern(..),

  pattern MkExprBinary,
  pattern MkExprString,
  pattern MkExprTuple,

  -- Re-exports
  module Lit,
  module Ann,
  module Pos,
  module Ty,

  -- Type families
  HLIR,
  TLIR,

  hlirToJSON,
  programToJSON,
  jsonToHLIR,
  jsonToProgram
) where
import Language.Bonzai.Syntax.Internal.Literal as Lit
import Language.Bonzai.Syntax.Internal.Annotation as Ann
import Language.Bonzai.Syntax.Internal.Position as Pos
import Language.Bonzai.Syntax.Internal.Type as Ty
import qualified Data.Text as T
import GHC.TypeLits (Symbol)
import Prelude hiding (Type)
import GHC.Show qualified as S
import Data.Aeson
import qualified Relude as BS

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
  | MkExprApplication (Expression f t) [Expression f t] (f t)
  | MkExprLambda [Annotation (f t)] (f t) (Expression f t)
  | MkExprTernary (Expression f t) (Expression f t) (Expression f t) (f t)
  | MkExprUpdate (Update f t) (Expression f t)
  | MkExprLet (Set Text) (Annotation (f t)) (Expression f t)
  | MkExprMut (Expression f t) (f t)
  | MkExprBlock [Expression f t] (f t)
  | MkExprActor t [Expression f t]
  | MkExprOn Text [Annotation (f t)] (Expression f t)
  | MkExprSend (Expression f t) Text [Expression f t] (f t)
  | MkExprRequire Text (Set Text)
  | MkExprLoc (Expression f t) Position
  | MkExprSpawn (Expression f t)
  | MkExprList [Expression f t]
  | MkExprNative (Annotation [Text]) Ty.Type
  | MkExprInterface (Annotation [QuVar]) [Annotation t]
  | MkExprWhile (Expression f t) (Expression f t)
  | MkExprIndex (Expression f t) (Expression f t)
  | MkExprData (Annotation [Text]) [DataConstructor t]
  | MkExprMatch (Expression f t) (f t) [(Pattern f t, Expression f t, Position)] (f t)
  | MkExprLive (Annotation (f t)) (Expression f t)
  | MkExprUnwrapLive (Expression f t) (f t)
  | MkExprWrapLive (Expression f t) (f t)
  | MkExprPublic (Expression f t)
  | MkExprTryCatch (Expression f t) (Annotation (f t)) (Expression f t)
  deriving Generic

-- | DATA CONSTRUCTOR TYPE
-- | Data constructor type is used to represent data constructors in Bonzai. It is
-- | used to represent constructors of data types.
data DataConstructor t
  = MkDataVariable Text
  | MkDataConstructor Text [t]
  deriving Generic

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

instance (ToJSON (f t), ToJSON t) => ToJSON (Expression f t)

instance (ToJSON (f t), ToJSON t) => ToJSON (Update f t)

instance (ToJSON t) => ToJSON (DataConstructor t)

instance (ToJSON (f t), ToJSON t) => ToJSON (Pattern f t)

instance (FromJSON (f t), FromJSON t) => FromJSON (Expression f t)

instance (FromJSON (f t), FromJSON t) => FromJSON (Update f t)

instance (FromJSON t) => FromJSON (DataConstructor t)

instance (FromJSON (f t), FromJSON t) => FromJSON (Pattern f t)

instance (ToText t, ToText (f t)) => Show (Expression f t) where
  show = T.unpack . toText

-- | BINARY EXPRESSION PATTERN
-- | A pattern synonym to represent binary expressions in Bonzai.
pattern MkExprBinary :: Text -> Expression Maybe t -> Expression Maybe t -> Expression Maybe t
pattern MkExprBinary op a b = MkExprApplication (MkExprVariable (MkAnnotation op Nothing)) [a, b] Nothing

-- | STRING EXPRESSION PATTERN
-- | A pattern synonym to represent string expressions in Bonzai.
pattern MkExprString :: Text -> Expression f t
pattern MkExprString s = MkExprLiteral (MkLitString s)

-- | TUPLE EXPRESSION PATTERN
-- | A pattern synonym to represent tuple expressions in Bonzai.
pattern MkExprTuple :: Expression Maybe t -> Expression Maybe t -> Expression Maybe t
pattern MkExprTuple a b = MkExprApplication (MkExprVariable (MkAnnotation "Tuple" Nothing)) [a, b] Nothing

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
  toText (MkExprApplication e es _) = T.concat ["(", toText e, ")(", T.intercalate ", " (map toText es), ")"]
  toText (MkExprLambda as ret e) = T.concat ["(", T.intercalate ", " (map toText as), "): ", toText ret, " => ", toText e]
  toText (MkExprTernary c t e _) = T.concat [toText c, " ? ", toText t, " : ", toText e]
  toText (MkExprUpdate u e) = T.concat [toText u, " = ", toText e]
  toText (MkExprLet _ a e) = T.concat ["let ", toText a, " = ", toText e]
  toText (MkExprBlock es _) = T.concat [T.intercalate "; " (map toText es)]
  toText (MkExprActor i e) = T.concat ["event ", toText i , " ", toText e]
  toText (MkExprOn n as e) = T.concat ["on ", n, "(", T.intercalate ", " (map toText as), ") { ", toText e, " }"]
  toText (MkExprSend e n e' _) = T.concat ["(", toText e, ") -> ", n, "(", toText e', ")"]
  toText (MkExprRequire n vars) = T.concat ["require ", n, ": ", T.intercalate ", " (toList vars)]
  toText (MkExprLoc e _) = toText e
  toText (MkExprSpawn e) = T.concat ["spawn ", toText e]
  toText (MkExprList es) = T.concat ["[", T.intercalate ", " (map toText es), "]"]
  toText (MkExprNative ann ty) = T.concat ["native ", toText ann.name, "<", T.intercalate ", " ann.value, "> ", toText ty]
  toText (MkExprMut e _) = T.concat ["mut ", toText e]
  toText (MkExprInterface ann as) = T.concat ["interface ", toText ann.name, "<", T.intercalate ", " (map toText as), ">"]
  toText (MkExprWhile c e) = T.concat ["while ", toText c, " { ", toText e, " }"]
  toText (MkExprIndex e e') = T.concat [toText e, "[", toText e', "]"]
  toText (MkExprData ann cs) = T.concat ["data ", toText ann.name, "<", T.intercalate ", " (map toText cs), ">"]
  toText (MkExprMatch e _ cs _) = T.concat ["match ", toText e, " { ", T.intercalate ", " (map (\(c, b, _) -> T.concat [toText c, " => ", toText b]) cs), " }"]
  toText (MkExprLive a e) = T.concat ["live ", toText a, " = ", toText e]
  toText (MkExprUnwrapLive e _) = T.concat ["unwrap ", toText e]
  toText (MkExprWrapLive e _) = T.concat ["wrap ", toText e]
  toText (MkExprPublic e) = T.concat ["pub ", toText e]
  toText (MkExprTryCatch e _ e') = T.concat ["try ", toText e, " catch ", toText e']

instance ToText t => ToText (DataConstructor t) where
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
  MkExprApplication e es t == MkExprApplication e' es' t' = e == e' && es == es' && t == t'
  MkExprLambda as ret e == MkExprLambda as' ret' e' = as == as' && ret == ret' && e == e'
  MkExprTernary c t e ty == MkExprTernary c' t' e' ty' = c == c' && t == t' && e == e' && ty == ty'
  MkExprUpdate u e == MkExprUpdate u' e' = u == u' && e == e'
  MkExprLet g a e == MkExprLet g' a' e' = a == a' && e == e' && g == g'
  MkExprMut e t == MkExprMut e' t' = e == e'  && t == t'
  MkExprBlock es t == MkExprBlock es' t' = es == es' && t == t'
  MkExprActor i e == MkExprActor i' e' = i == i' && e == e'
  MkExprOn n as e == MkExprOn n' as' e' = n == n' && as == as' && e == e'
  MkExprSend e n es t == MkExprSend e' n' es' t' = e == e' && n == n' && es == es' && t == t'
  MkExprRequire n v == MkExprRequire n' v' = n == n' && v == v'
  MkExprLoc e _ == MkExprLoc e' _ = e == e'
  MkExprLoc e _ == e' = e == e'
  e == MkExprLoc e' _ = e == e'
  MkExprSpawn e == MkExprSpawn e' = e == e'
  MkExprList es == MkExprList es' = es == es'
  MkExprNative ann ty == MkExprNative ann' ty' = ann == ann' && ty == ty'
  MkExprInterface ann as == MkExprInterface ann' as' = ann == ann' && as == as'
  MkExprWhile c e == MkExprWhile c' e' = c == c' && e == e'
  MkExprIndex e e' == MkExprIndex e'' e''' = e == e'' && e' == e'''
  MkExprData ann cs == MkExprData ann' cs' = ann == ann' && cs == cs'
  MkExprMatch e t cs t2 == MkExprMatch e' t' cs' t2' = e == e' && cs == cs' && t == t' && t2 == t2'
  MkExprLive a e == MkExprLive a' e' = a == a' && e == e'
  MkExprUnwrapLive e t == MkExprUnwrapLive e' t' = e == e' && t == t'
  MkExprWrapLive e t == MkExprWrapLive e' t' = e == e' && t == t'
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

hlirToJSON :: (ToJSON (f t), ToJSON t) => Expression f t -> Value
hlirToJSON = toJSON

programToJSON :: (ToJSON (f t), ToJSON t) => [Expression f t] -> String
programToJSON = BS.decodeUtf8 . encode

jsonToHLIR :: (FromJSON (f t), FromJSON t) => String -> Maybe (Expression f t)
jsonToHLIR = decode . BS.encodeUtf8

jsonToProgram :: (FromJSON (f t), FromJSON t) => String -> Maybe [Expression f t]
jsonToProgram = decode . BS.encodeUtf8