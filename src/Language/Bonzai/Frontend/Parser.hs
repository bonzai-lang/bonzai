module Language.Bonzai.Frontend.Parser (
    module P,
    module M,
    module MCL,
    ParseError,
    parseBonzaiFile,
    parseBonzaiTestFile,
) where

import Control.Monad.Parser as P
import Text.Megaparsec as M hiding (ParseError)
import Text.Megaparsec.Char.Lexer as MCL

type ParseError = ParseErrorBundle Text Void

parseBonzaiFile ::
    (MonadIO m) =>
    FilePath ->
    FileContent ->
    P.Parser m a ->
    m (Either ParseError a)
parseBonzaiFile filePath fileContent p =
    P.parseContent p filePath fileContent

parseBonzaiTestFile ::
    (MonadIO m) =>
    FileContent ->
    P.Parser m a ->
    m (Either ParseError a)
parseBonzaiTestFile = flip P.parseTestContent
