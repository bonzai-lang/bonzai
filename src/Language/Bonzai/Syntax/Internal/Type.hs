{-# LANGUAGE PatternSynonyms #-}
module Language.Bonzai.Syntax.Internal.Type where

import Prelude hiding (Type)
import qualified GHC.IO as IO
import qualified Data.Text as T

-- Level represents the level of a type variable. It is used to determine the
-- scope of a type variable.
type Level = Int

-- QuVar represents a generic type defined by the user. For instance "A" in
-- the following example: "fn id<A>(x: A): A => x".
type QuVar = Text

data Type 
  = MkTyId Text
  | MkTyApp Type [Type]
  | MkTyVar (IORef TyVar)
  | MkTyQuantified Text

-- Type variable represents a type variable in Plume. It can either be a link to
-- another type or an unbound type variable.
data TyVar
  = Link Type
  | Unbound QuVar Level
  deriving (Eq)

data Scheme = Forall [QuVar] Type
  deriving (Eq)

instance Eq Type where
  MkTyId a == MkTyId b = a == b
  MkTyVar a == MkTyVar b = do
    let a' = IO.unsafePerformIO $ readIORef a
    let b' = IO.unsafePerformIO $ readIORef b
    a' == b'
  MkTyApp a b == MkTyApp c d = a == c && b == d
  _ == _ = False

pattern MkTyFun :: [Type] -> Type -> Type
pattern MkTyFun args retTy = MkTyApp (MkTyId "#func") (retTy : args)

pattern (:->:) :: [Type] -> Type -> Type
pattern args :->: retTy = MkTyFun args retTy

pattern MkTyInt, MkTyFloat, MkTyChar, MkTyString, MkTyBool, MkTyUnit :: Type
pattern MkTyInt = MkTyId "int"
pattern MkTyFloat = MkTyId "float"
pattern MkTyChar = MkTyId "char"
pattern MkTyString = MkTyId "string"
pattern MkTyBool = MkTyId "bool"
pattern MkTyUnit = MkTyId "unit"

pattern MkTyList :: Type -> Type
pattern MkTyList a = MkTyApp (MkTyId "list") [a]

pattern MkTyMutable :: Type -> Type
pattern MkTyMutable a = MkTyApp (MkTyId "mutable") [a]

instance ToText Type where 
  toText (MkTyId a) = a
  toText (args :->: ret) = T.concat ["(", T.intercalate ", " (map toText args), ") -> ", toText ret]
  toText (MkTyApp a b) = T.concat [toText a, "<", T.intercalate ", " (map toText b), ">"]
  toText (MkTyVar a) = do
    let a' = IO.unsafePerformIO $ readIORef a
    toText a'
  toText (MkTyQuantified a) = a

instance ToText TyVar where
  toText (Link a) = toText a
  toText (Unbound a l) = a <> "@" <> T.pack (show l)

instance ToText (Maybe Type) where
  toText (Just a) = toText a
  toText Nothing = "infer"

instance ToText Scheme where
  toText (Forall a b) = T.concat ["forall ", T.intercalate ", " a, ". ", toText b]

instance ToText (Identity Type) where
  toText (Identity a) = toText a