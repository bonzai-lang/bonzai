module Language.Bonzai.Frontend.Parser.Internal.Literal where
import qualified Language.Bonzai.Frontend.Parser as P
import qualified Text.Megaparsec.Char as MC
import qualified Data.Text as T
import qualified Language.Bonzai.Syntax.HLIR as HLIR
import Text.Megaparsec.Char (digitChar)

decimal :: (MonadIO m) => P.Parser m Integer
decimal = do
  parts :: String <- concat <$> P.sepBy1 (some digitChar) (MC.char '_')

  case readEither parts of
    Left _ -> fail "Invalid integer"
    Right x -> pure x

parseInteger :: (MonadIO m) => P.Parser m Integer
parseInteger = P.signed (pure ()) decimal

parseFloat :: (MonadIO m) => P.Parser m Double
parseFloat = P.signed (pure ()) $ do
  int <- concat <$> P.sepBy1 (some digitChar) (MC.char '_')
  void $ MC.char '.'
  frac <- concat <$> P.sepBy1 (some digitChar) (MC.char '_')

  case readEither (int <> "." <> frac) of
    Left _ -> fail "Invalid float"
    Right y -> pure y

parseChar :: (MonadIO m) => P.Parser m Char
parseChar = MC.char '\'' *> P.charLiteral <* MC.char '\''

stringUnitLiteral :: (MonadIO m) => P.Parser m [Char]
stringUnitLiteral = (:[]) <$> P.charLiteral <|> MC.string "\\$" $> "\\$"

parseString :: (MonadIO m) => P.Parser m Text
parseString = MC.char '"' *> P.manyTill stringUnitLiteral (MC.char '"') <&> T.pack . concat

parseLiteral :: (MonadIO m) => P.Parser m HLIR.Literal
parseLiteral = P.choice [
    P.try $ HLIR.MkLitFloat <$> parseFloat,
    P.try $ HLIR.MkLitInt <$> parseInteger,
    HLIR.MkLitChar <$> parseChar
  ]