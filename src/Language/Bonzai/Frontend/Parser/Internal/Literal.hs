module Language.Bonzai.Frontend.Parser.Internal.Literal where
import qualified Language.Bonzai.Frontend.Parser as P
import qualified Text.Megaparsec.Char as MC
import qualified Data.Text as T
import qualified Language.Bonzai.Syntax.HLIR as HLIR

parseInteger :: (MonadIO m) => P.Parser m Integer
parseInteger = P.signed (pure ()) P.decimal

parseFloat :: (MonadIO m) => P.Parser m Double
parseFloat = P.signed (pure ()) P.float

parseChar :: (MonadIO m) => P.Parser m Char
parseChar = MC.char '\'' *> P.charLiteral <* MC.char '\''

parseString :: (MonadIO m) => P.Parser m Text
parseString = MC.char '"' *> P.manyTill P.charLiteral (MC.char '"') <&> T.pack

parseLiteral :: (MonadIO m) => P.Parser m HLIR.Literal
parseLiteral = P.choice [
    P.try $ HLIR.MkLitFloat <$> parseFloat,
    P.try $ HLIR.MkLitInt <$> parseInteger,
    HLIR.MkLitChar <$> parseChar,
    HLIR.MkLitString <$> parseString
  ]