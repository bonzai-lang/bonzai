{-# LANGUAGE PatternSynonyms #-}
module Language.Bonzai.Syntax.CLang where
import Language.Bonzai.Syntax.TMLIR (Literal)
import qualified Data.Text as Text
import Data.Char (isPrint, isLetter, isAlphaNum)
import Text.Printf (printf)
import Prelude hiding (Type)

type Type = Text

data Expression 
  = MkExprLiteral Literal
  | MkExprVariable Text
  | MkExprApplication Expression [Expression]
  | MkExprIndex Expression Expression
  | MkExprField Expression Text
  | MkExprUpdate Update Expression
  | MkExprBlock [Statement] Type
  | MkExprList [Expression]
  | MkExprTernary Expression Expression Expression Type
  | MkExprStruct Text [(Maybe Text, Expression)]
  | MkExprRef Expression
  | MkExprUnref Expression
  | MkExprCast Expression Type
  | MkExprSizeOf Type
  deriving (Eq)

data Update
  = MkUpdtVariable Text Type
  | MkUpdtField Update Text
  | MkUpdtIndex Update Expression
  | MkUpdtUnref Update
  deriving (Eq)

data Statement
  = MkStmtExpr Expression
  | MkStmtIf Expression [Statement] [Statement]
  | MkStmtWhile Expression [Statement]
  | MkStmtFor Expression Expression Expression [Statement]
  | MkStmtReturn Expression
  | MkStmtBreak
  | MkStmtContinue
  | MkStmtBlock [Statement]
  | MkStmtDeclare Text Type
  | MkStmtAssign Expression Expression
  | MkStmtDeclareAssign Text Type Expression
  deriving (Eq)

data Toplevel 
  = MkTopFunction Text [(Text, Type)] Type [Statement]
  | MkTopStruct Text [Structure]
  | MkTopUnion Text [Structure]
  | MkTopEnum Text [Text]
  | MkTopExtern Text [Type] Type
  | MkTopTypedef Text [Type] Type
  | MkTopForwardDecl Text [Type] Type
  deriving (Eq)

data Structure
  = MkStructField Text Type
  | MkStructStruct Text [Structure] Text
  | MkStructUnion Text [Structure]
  deriving (Eq)

pattern MkExprCall :: Text -> [Expression] -> Expression
pattern MkExprCall f args = MkExprApplication (MkExprVariable f) args

instance ToText Expression where
  toText (MkExprLiteral l) = toText l
  toText (MkExprVariable v) = varify' v
  toText (MkExprCall "==" [a, b]) = toText a <> " == " <> toText b
  toText (MkExprCall "!=" [a, b]) = toText a <> " != " <> toText b
  toText (MkExprCall "&&" [a, b]) = toText a <> " && " <> toText b
  toText (MkExprCall "||" [a, b]) = toText a <> " || " <> toText b
  toText (MkExprCall "!" [a]) = "!" <> toText a
  toText (MkExprCall ">" [a, b]) = toText a <> " > " <> toText b
  toText (MkExprCall "<" [a, b]) = toText a <> " < " <> toText b
  toText (MkExprCall ">=" [a, b]) = toText a <> " >= " <> toText b
  toText (MkExprCall "<=" [a, b]) = toText a <> " <= " <> toText b
  toText (MkExprCall "+" [a, b]) = toText a <> " + " <> toText b
  toText (MkExprCall "-" [a, b]) = toText a <> " - " <> toText b
  toText (MkExprCall "*" [a, b]) = toText a <> " * " <> toText b
  toText (MkExprCall "/" [a, b]) = toText a <> " / " <> toText b
  toText (MkExprCall "%" [a, b]) = toText a <> " % " <> toText b
  toText (MkExprApplication f args) = toText f <> "(" <> Text.intercalate ", " (map toText args) <> ")"
  toText (MkExprIndex e i) = toText e <> "[" <> toText i <> "]"
  toText (MkExprField e f) = toText e <> "." <> varify' f
  toText (MkExprUpdate u e) = toText u <> " = " <> toText e
  toText (MkExprBlock es _) = "{\n" <> Text.intercalate ";\n" (map toText es) <> ";\n}"
  toText (MkExprList es) = "{" <> Text.intercalate ", " (map toText es) <> "}"
  toText (MkExprTernary c t e _) = toText c <> " ? " <> toText t <> " : " <> toText e
  toText (MkExprStruct "" anns) = "{" <> Text.intercalate ", " (map (\(n', e) -> case n' of
      Just n -> "." <> varify' n <> " = " <> toText e
      Nothing -> toText e
    ) anns) <> "}"
  toText (MkExprStruct n anns) = "(struct " <> varify' n <> ") {" <> Text.intercalate ", " (map (\(n', e) -> case n' of
      Just n'' -> "." <> varify' n'' <> " = " <> toText e
      Nothing -> toText e
    ) anns) <> "}"
  toText (MkExprRef e) = "(&" <> toText e <> ")"
  toText (MkExprUnref e) = "(*" <> toText e <> ")"
  toText (MkExprCast e t) = "(" <> toText t <> ")" <> toText e
  toText (MkExprSizeOf t) = "sizeof(" <> toText t <> ")"

instance ToText Update where
  toText (MkUpdtVariable v _) = varify' v
  toText (MkUpdtField u f) = toText u <> "." <> varify' f
  toText (MkUpdtIndex u e) = toText u <> "[" <> toText e <> "]"
  toText (MkUpdtUnref u) = "*" <> toText u

instance ToText Statement where
  toText (MkStmtExpr e) = toText e <> ";"
  toText (MkStmtIf c t e) = "if (" <> toText c <> ") " <> toText t <> " else " <> toText e
  toText (MkStmtWhile c b) = "while (" <> toText c <> ") " <> toText b
  toText (MkStmtFor i c u b) = "for (" <> toText i <> "; " <> toText c <> "; " <> toText u <> ") " <> toText b
  toText (MkStmtReturn e) = "return " <> toText e <> ";"
  toText MkStmtBreak = "break;"
  toText MkStmtContinue = "continue;"
  toText (MkStmtBlock ss) = "{\n" <> Text.intercalate ";\n" (map toText ss) <> ";\n}"
  toText (MkStmtDeclare v t) = toText t <> " " <> varify' v <> ";"
  toText (MkStmtAssign v e) = toText v <> " = " <> toText e <> ";"
  toText (MkStmtDeclareAssign v t e) = toText t <> " " <> varify' v <> " = " <> toText e <> ";"

instance ToText Toplevel where
  toText (MkTopFunction n args t ss) = toText t <> " " <> varify' n <> "(" <> Text.intercalate ", " (map (\(a, t') -> toText t' <> " " <> varify' a) args) <> ") {" <> toText ss <> "}"
  toText (MkTopStruct n ss) = "struct " <> varify' n <> " {" <> Text.intercalate ";" (map toText ss) <> "};"
  toText (MkTopUnion n ss) = "union " <> varify' n <> " {" <> toText ss <> "};"
  toText (MkTopEnum n es) = "enum " <> varify' n <> " " <> "{" <> Text.intercalate ", " es <> "};"
  toText (MkTopExtern n args ret) = "extern " <> toText ret <> " " <> varify' n <> "(" <> Text.intercalate ", " (map toText args) <> ");"
  toText (MkTopTypedef n args ret) = "typedef " <> toText ret <> " (*" <> varify' n <> ")(" <> Text.intercalate ", " (map toText args) <> ");"
  toText (MkTopForwardDecl n args ret) = toText ret <> " " <> varify' n <> "(" <> Text.intercalate ", " (map toText args) <> ");"

instance ToText Structure where
  toText (MkStructField n t) = toText t <> " " <> varify' n <> ";"
  toText (MkStructStruct "" ss f) = "struct {" <> Text.intercalate ";" (map toText ss) <> "} " <> varify' f <> ";"
  toText (MkStructStruct n ss f) = "struct " <> varify' n <> " {" <> Text.intercalate ";" (map toText ss) <> "} " <> varify' f <> ";"
  toText (MkStructUnion "" ss) = "union {" <> Text.intercalate ";" (map toText ss) <> "};"
  toText (MkStructUnion n ss) = "union " <> varify' n <> " {" <> Text.intercalate ";" (map toText ss) <> "};"

instance ToText [Toplevel] where
  toText = Text.intercalate ";\n" . map toText

instance ToText [Structure] where
  toText = Text.intercalate ";\n" . map toText

instance ToText [Statement] where
  toText sts = "{" <> Text.intercalate ";\n" (map toText sts) <> "}"

isIdent :: Char -> Bool
isIdent x = isAlphaNum x || x == '_' || x == '$'

varify :: String -> String
varify "" = ""
varify x | isValidIdent x = x
varify x = "$" <> concatMap (\c -> if isIdent c then [c] else show (ord c)) x

varify' :: Text -> Text
varify' = fromString . varify . toString

isValidIdent :: String -> Bool
isValidIdent (x:xs) = (isLetter x || x == '_' || x == '$') && all isIdent xs
isValidIdent _ = False
 
encodeUnicode16 :: String -> String
encodeUnicode16 = concatMap escapeChar
  where
    escapeChar c
      | c == '\"' = "\\\""
      | c == '\'' = "\\\'"
      | ' ' <= c && c <= 'z' = [c]
      | isPrint c = [c]
      | otherwise = printf "\\u%04x" (fromEnum c)