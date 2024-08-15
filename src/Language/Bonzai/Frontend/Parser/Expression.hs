module Language.Bonzai.Frontend.Parser.Expression where

import qualified Language.Bonzai.Frontend.Parser as P
import qualified Language.Bonzai.Frontend.Parser.Lexer as Lex
import qualified Language.Bonzai.Frontend.Parser.Internal.Literal as Lit
import qualified Language.Bonzai.Syntax.HLIR as HLIR
import qualified Language.Bonzai.Frontend.Parser.Internal.Type as Typ

localize :: (MonadIO m, HLIR.Locate a) => P.Parser m a -> P.Parser m a
localize p = do
  start <- P.getSourcePos
  x <- p
  end <- P.getSourcePos

  pure $ HLIR.locate x (start, end)

parseLiteral :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseLiteral = localize . Lex.lexeme $ HLIR.MkExprLiteral <$> Lit.parseLiteral

parseList :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseList = localize $ do
  void $ Lex.symbol "["
  exprs <- P.many parseExpression
  void $ Lex.symbol "]"

  pure $ HLIR.MkExprList exprs

parseNative :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseNative = localize $ do
  void $ Lex.reserved "native"
  name <- Lex.identifier

  gens <- P.option [] $ Lex.brackets (P.many Lex.identifier)

  HLIR.MkExprNative (HLIR.MkAnnotation name gens) <$> Typ.parseType

parseVariable :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseVariable = localize $ do
  name <- Lex.lexeme Lex.identifier

  pure $ HLIR.MkExprVariable $ HLIR.MkAnnotation name Nothing

parseLet :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseLet = localize $ do
  void $ Lex.reserved "let"
  name <- Lex.identifier

  HLIR.MkExprLet (HLIR.MkAnnotation name Nothing) <$> parseExpression

parseMut :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseMut = localize $ do
  void $ Lex.reserved "mut"
  name <- Lex.identifier

  HLIR.MkExprMut (HLIR.MkAnnotation name Nothing) <$> parseExpression

parseModule :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseModule = localize $ do
  void $ Lex.reserved "module"
  name <- Lex.identifier

  HLIR.MkExprModule name <$> P.many parseToplevel

parseBlock :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseBlock = localize $ do
  void $ Lex.symbol "{"
  exprs <- P.many parseExpression
  void $ Lex.symbol "}"

  pure $ HLIR.MkExprBlock exprs

parseFunction :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseFunction = localize $ do
  void $ Lex.reserved "defn"
  name <- Lex.identifier
  args <- Lex.brackets (P.many Lex.identifier)

  HLIR.MkExprLet (HLIR.MkAnnotation name Nothing) . HLIR.MkExprLambda (map (`HLIR.MkAnnotation` Nothing) args) Nothing <$> parseExpression

parseLambda :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseLambda = localize $ do
  void $ Lex.reserved "fn"
  args <- Lex.brackets (P.many Lex.identifier)

  HLIR.MkExprLambda (map (`HLIR.MkAnnotation` Nothing) args) Nothing <$> parseExpression

parseUpdate :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseUpdate = localize $ do
  void $ Lex.reserved "set"
  update <- parseUpdate'

  HLIR.MkExprUpdate update <$> parseExpression

  where
    parseUpdate' = P.choice [
        parseUpdateVar,
        parseUpdateField,
        parseUpdateIndex
      ]

    parseUpdateVar = do
      name <- Lex.identifier
      pure $ HLIR.MkUpdtVariable $ HLIR.MkAnnotation name Nothing

    parseUpdateField = Lex.parens $ do
      update <- parseUpdate'
      void $ Lex.symbol "."

      HLIR.MkUpdtField update <$> Lex.identifier

    parseUpdateIndex = Lex.parens $ do
      void $ Lex.reserved "get"
      update <- parseUpdate'

      HLIR.MkUpdtIndex update <$> parseExpression

parseApplication :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseApplication = localize $ do
  expr <- parseExpression
  args <- P.many parseExpression

  pure $ HLIR.MkExprApplication expr args

parseEvent :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseEvent = localize $ do
  void $ Lex.reserved "event"
  name <- Lex.identifier
  HLIR.MkExprLet (HLIR.MkAnnotation name Nothing) . HLIR.MkExprEvent <$> P.many parseExpression

parseAnonEvent :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseAnonEvent = localize $ do
  void $ Lex.reserved "event"
  HLIR.MkExprEvent <$> P.many parseExpression

parseOn :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseOn = localize $ do
  void $ Lex.reserved "on"
  name <- Lex.identifier
  args <- Lex.brackets (P.many Lex.identifier)

  HLIR.MkExprOn name (map (`HLIR.MkAnnotation` Nothing) args) <$> parseExpression

parseSpawn :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseSpawn = localize $ do
  void $ Lex.reserved "spawn"
  HLIR.MkExprSpawn <$> parseExpression

parseSend :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseSend = localize $ do
  void $ Lex.reserved "send"
  expr <- parseExpression
  name <- Lex.identifier
  
  HLIR.MkExprSend expr name <$> P.many parseExpression

parseRequire :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseRequire = localize $ do
  void $ Lex.reserved "require"

  HLIR.MkExprRequire <$> Lit.parseString

parseExpression :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseExpression =
  Lex.parens (P.choice [
    parseLet,
    parseMut,
    parseFunction,
    parseUpdate,
    P.try parseEvent,
    parseAnonEvent,
    parseOn,
    parseLambda,
    parseSend,
    parseSpawn,
    parseApplication
  ]) <|>
    P.choice [
      parseLiteral,
      parseBlock,
      parseVariable,
      parseList
    ]

parseToplevel :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseToplevel = 
  P.try (Lex.parens (P.choice [
    parseModule,
    parseRequire,
    parseNative
  ])) <|>
    parseExpression

parseProgram :: MonadIO m => P.Parser m [HLIR.HLIR "expression"]
parseProgram = P.many parseToplevel