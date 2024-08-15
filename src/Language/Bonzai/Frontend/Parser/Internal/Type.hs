module Language.Bonzai.Frontend.Parser.Internal.Type where
import qualified Language.Bonzai.Frontend.Parser as P
import qualified Language.Bonzai.Syntax.HLIR as HLIR
import qualified Language.Bonzai.Frontend.Parser.Lexer as Lex

parseType :: (MonadIO m) => P.Parser m HLIR.Type
parseType = 
  P.choice [
    Lex.identifier <&> HLIR.MkTyId,
    P.try . Lex.parens $ do
      void $ Lex.symbol "->"
      tys <- P.many parseType

      case unsnoc tys of
        Just (tys', ty2) -> pure $ tys' HLIR.:->: ty2
        Nothing -> fail "parseType: expected at least one type on the left side of the arrow",
    Lex.parens $ do
      tys <- P.many parseType

      case uncons tys of
        Just (ty, tys') -> pure $ HLIR.MkTyApp ty tys'
        Nothing -> fail "parseType: expected at least one type in the application"
  ]

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc xs = do
  ys <- viaNonEmpty init xs
  z <- viaNonEmpty last xs

  pure (ys, z)