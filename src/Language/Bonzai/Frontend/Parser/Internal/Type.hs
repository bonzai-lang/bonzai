module Language.Bonzai.Frontend.Parser.Internal.Type where
import qualified Language.Bonzai.Frontend.Parser as P
import qualified Language.Bonzai.Syntax.HLIR as HLIR
import qualified Language.Bonzai.Frontend.Typechecking.Monad as M
import qualified Language.Bonzai.Frontend.Parser.Lexer as Lex
import qualified Data.List as List

-- | TYPE
-- | Parse a type.
parseType :: (MonadIO m) => P.Parser m HLIR.Type
parseType =
  P.choice [
    Lex.braces $ do
      -- Record type constructor
      -- Defined as the following:
      --
      -- "{" (identifier ":" type (",")?)* "}"
      fields <- flip P.sepBy Lex.comma $ do
        idt <- Lex.identifier
        opt <- P.option False $ Lex.symbol "?" $> True
        void $ Lex.symbol ":"
        (opt,idt,) <$> parseType

      let fields' = List.foldl (\acc (o, k, v) -> HLIR.MkTyRowExtend k v o acc) HLIR.MkTyRowEmpty fields

      pure $ HLIR.MkTyRecord fields',

    -- Function type constructor
    -- Defined as the following:
    --
    -- "fn" "(" type ("," type)* ")" ":" type
    do
      void $ Lex.reserved "fn"
      tys <- Lex.parens $ P.sepBy parseType Lex.comma
      ret <- Lex.symbol ":" *> parseType

      kwarg <- M.fresh

      pure $ (tys ++ [kwarg]) HLIR.:->: ret,

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
