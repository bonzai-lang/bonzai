module Language.Bonzai.Frontend.Parser.Internal.Type where
import qualified Language.Bonzai.Frontend.Parser as P
import qualified Language.Bonzai.Syntax.HLIR as HLIR
import qualified Language.Bonzai.Frontend.Parser.Lexer as Lex

-- | TYPE
-- | Parse a type.
parseType :: (MonadIO m) => P.Parser m HLIR.Type
parseType =
  P.choice [
    -- Function type constructor
    -- Defined as the following:
    --
    -- "fn" "(" type ("," type)* ")" ":" type
    do
      void $ Lex.reserved "fn"
      tys <- Lex.parens $ P.sepBy parseType Lex.comma
      ret <- Lex.symbol ":" *> parseType

      pure $ tys HLIR.:->: ret,

    -- Actor type constructor
    -- Defined as the following:
    --
    -- "actor" type
    do
      void $ Lex.reserved "actor"
      HLIR.MkTyActor <$> parseType,

    -- Mutable type constructor
    -- Defined as the following:
    --
    -- "mut" type
    do
      void $ Lex.reserved "mut"
      HLIR.MkTyMutable <$> parseType,

    -- Tuple type constructor
    -- Defined as the following:
    --
    -- "(" type "," type ")"
    Lex.parens $ do
      x <- parseType
      void $ Lex.reserved ","
      HLIR.MkTyTuple x <$> parseType,

    -- Type application constructor
    -- Defined as the following:
    --
    -- identifier "<" type ("," type)* ">"
    P.try $ do
      idt <- Lex.identifier
      tys <- Lex.angles $ P.sepBy1 parseType Lex.comma

      pure $ HLIR.MkTyApp (HLIR.MkTyId idt) tys,
    Lex.identifier <&> HLIR.MkTyId
  ]
