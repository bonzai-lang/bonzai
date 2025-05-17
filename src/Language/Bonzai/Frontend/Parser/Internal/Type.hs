module Language.Bonzai.Frontend.Parser.Internal.Type where

import Data.List qualified as List
import Language.Bonzai.Frontend.Parser qualified as P
import Language.Bonzai.Frontend.Parser.Lexer qualified as Lex
import Language.Bonzai.Frontend.Typechecking.Monad qualified as M
import Language.Bonzai.Syntax.HLIR qualified as HLIR

-- | TYPE
-- | Parse a type.
parseType :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.Type)
parseType =
  P.choice
    [ do
        (pos, obj) <- Lex.braces $ do
          -- Record type constructor
          -- Defined as the following:
          --
          -- "{" (identifier ":" type (",")?)* "}"
          fields <- flip P.sepBy Lex.comma $ do
            (_, idt) <- Lex.identifier
            opt <- P.option False $ Lex.symbol "?" $> True
            void $ Lex.symbol ":"
            (opt,idt,) . snd <$> parseType

          let fields' = List.foldl (\acc (o, k, v) -> HLIR.MkTyRowExtend k v o acc) HLIR.MkTyRowEmpty fields

          pure $ HLIR.MkTyRecord fields'
        pure (pos, obj),
      -- Function type constructor
      -- Defined as the following:
      --
      -- "fn" "(" type ("," type)* ")" ":" type
      do
        ((start, _), _) <- Lex.reserved "fn"
        tys <- snd <$> Lex.parens (P.sepBy (snd <$> parseType) Lex.comma)
        ((_, end), ret) <- Lex.symbol ":" *> parseType

        kwarg <- M.fresh

        pure $ ((start, end), (tys ++ [kwarg]) HLIR.:->: ret),
      -- Mutable type constructor
      -- Defined as the following:
      --
      -- "mut" type
      do
        ((start, _), _) <- Lex.reserved "mut"
        ((_, end), obj) <- parseType
        let obj' = HLIR.MkTyMutable obj
        pure ((start, end), obj'),
      -- Tuple type constructor
      -- Defined as the following:
      --
      -- "(" type "," type ")"
      do
        (pos, ty) <- Lex.parens $ do
          x <- snd <$> parseType
          void $ Lex.reserved ","
          HLIR.MkTyTuple x . snd <$> parseType
        pure (pos, ty),
      -- Type application constructor
      -- Defined as the following:
      --
      -- identifier "<" type ("," type)* ">"
      P.try $ do
        ((start, _), idt) <- Lex.identifier
        ((_, end), tys) <- Lex.angles $ P.sepBy1 (snd <$> parseType) Lex.comma

        pure ((start, end), HLIR.MkTyApp (HLIR.MkTyId idt) tys),
      Lex.identifier <&> second HLIR.MkTyId
    ]
