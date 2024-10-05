module Language.Bonzai.Frontend.Parser.Expression where

import qualified Language.Bonzai.Frontend.Parser as P
import qualified Language.Bonzai.Frontend.Parser.Lexer as Lex
import qualified Language.Bonzai.Frontend.Parser.Internal.Literal as Lit
import qualified Language.Bonzai.Syntax.HLIR as HLIR
import qualified Language.Bonzai.Frontend.Parser.Internal.Type as Typ
import qualified Control.Monad.Combinators.Expr as P
import qualified Text.Megaparsec.Char as P

parseAnnotation :: MonadIO m => P.Parser m a -> P.Parser m (HLIR.Annotation (Maybe a))
parseAnnotation p = P.choice [
    P.try $ do
      name <- Lex.identifier
      ty <- P.optional (Lex.symbol ":" *> p)

      pure $ HLIR.MkAnnotation name ty,
    HLIR.MkAnnotation <$> Lex.identifier <*> pure Nothing
  ]

parseAnnotation' :: MonadIO m => P.Parser m a -> P.Parser m (HLIR.Annotation a)
parseAnnotation' p = HLIR.MkAnnotation <$> Lex.identifier <*> (Lex.symbol ":" *> p)

localize :: (MonadIO m, HLIR.Locate a) => P.Parser m a -> P.Parser m a
localize p = do
  start <- P.getSourcePos
  x <- p
  end <- P.getSourcePos

  pure $ HLIR.locate x (start, end)

parseLiteral :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseLiteral = localize . Lex.lexeme $ HLIR.MkExprLiteral <$> Lit.parseLiteral

parseTernary :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseTernary = localize $ do
  void $ Lex.reserved "if"
  cond <- parseExpression

  void $ Lex.reserved "then"
  then' <- parseExpression

  void $ Lex.reserved "else"
  
  HLIR.MkExprTernary cond then' <$> parseExpression

parseList :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseList = localize $ do
  void $ Lex.symbol "["
  exprs <- P.sepBy parseExpression Lex.comma
  void $ Lex.symbol "]"

  pure $ HLIR.MkExprList exprs

parseExtern :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseExtern = localize $ do
  void $ Lex.reserved "extern"

  P.choice [
      do
        void $ Lex.reserved "fn"
        name <- Lex.identifier <|> Lex.parens Lex.operator
        gens <- P.option [] $ Lex.angles (P.sepBy Lex.identifier Lex.comma)

        args <- Lex.parens $ P.sepBy (parseAnnotation' Typ.parseType) Lex.comma
        ret <- P.option HLIR.MkTyUnit $ Lex.symbol ":" *> Typ.parseType

        let funTy = map (.value) args HLIR.:->: ret

        pure $ HLIR.MkExprNative (HLIR.MkAnnotation name gens) funTy,

      do
        name <- Lex.identifier <|> Lex.parens Lex.operator
        gens <- P.option [] $ Lex.angles (P.sepBy Lex.identifier Lex.comma)

        ty <- Lex.symbol ":" *> Typ.parseType

        pure $ HLIR.MkExprNative (HLIR.MkAnnotation name gens) ty
    ]

parseVariable :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseVariable = localize $ do
  name <- Lex.lexeme Lex.identifier

  pure $ HLIR.MkExprVariable $ HLIR.MkAnnotation name Nothing

parseLet :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseLet = localize $ do
  void $ Lex.reserved "let"
  name <- Lex.identifier
  void $ Lex.reserved "="

  HLIR.MkExprLet (HLIR.MkAnnotation name Nothing) <$> parseExpression

parseMut :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseMut = localize $ do
  void $ Lex.reserved "mut"
  name <- Lex.identifier
  void $ Lex.reserved "="

  HLIR.MkExprMut (HLIR.MkAnnotation name Nothing) <$> parseExpression

parseModule :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseModule = localize $ do
  void $ Lex.reserved "module"
  name <- Lex.identifier

  HLIR.MkExprModule name <$> Lex.braces (P.many parseToplevel)

parseInterface :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseInterface = localize $ do
  void $ Lex.reserved "interface"
  name <- Lex.identifier
  gens <- P.option [] $ Lex.angles (P.sepBy Lex.identifier Lex.comma)

  HLIR.MkExprInterface (HLIR.MkAnnotation name gens) <$> Lex.braces (P.many parseDef)

  where
    parseDef :: MonadIO m => P.Parser m (HLIR.Annotation HLIR.Type)
    parseDef = do
      void $ Lex.reserved "fn"
      name <- Lex.identifier

      args <- P.option [] $ Lex.parens (P.sepBy (parseAnnotation' Typ.parseType) Lex.comma)

      let funTy = map (.value) args HLIR.:->: HLIR.MkTyUnit

      pure $ HLIR.MkAnnotation name funTy

parseBlock :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseBlock = localize $ do
  void $ Lex.symbol "{"
  exprs <- P.many parseExpression
  void $ Lex.symbol "}"

  pure $ HLIR.MkExprBlock exprs

parseFunction :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseFunction = localize $ do
  void $ Lex.reserved "fn"
  name <- Lex.identifier
  args <- Lex.parens (P.sepBy (parseAnnotation Typ.parseType) Lex.comma)

  void $ Lex.symbol "=>"

  HLIR.MkExprLet (HLIR.MkAnnotation name Nothing) . HLIR.MkExprLambda args Nothing <$> parseExpression

parseLambda :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseLambda = localize $ do
  void $ Lex.reserved "fn"
  args <- Lex.parens (P.sepBy (parseAnnotation Typ.parseType) Lex.comma)

  void $ Lex.symbol "=>"

  HLIR.MkExprLambda args Nothing <$> parseExpression

parseUpdate :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseUpdate = localize $ do
  name <- P.try $ Lex.identifier <* Lex.symbol "="

  HLIR.MkExprUpdate (HLIR.MkUpdtVariable (HLIR.MkAnnotation name Nothing)) <$> parseExpression

parseActor :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseActor = localize $ do
  void $ Lex.reserved "actor"
  name <- Lex.identifier
  implemented <- Lex.symbol "<" *> Lex.identifier
  HLIR.MkExprLet (HLIR.MkAnnotation name Nothing) 
    . HLIR.MkExprActor implemented 
      <$> Lex.braces (P.many parseEvent)

parseAnonActor :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseAnonActor = localize $ do
  void $ Lex.reserved "actor"
  implemented <- Lex.symbol "<" *> Lex.identifier
  HLIR.MkExprActor implemented <$> Lex.braces (P.many parseEvent)

parseEvent :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseEvent = localize $ do
  void $ Lex.reserved "on"
  name <- Lex.identifier
  args <- Lex.parens (P.sepBy (parseAnnotation Typ.parseType) Lex.comma)

  void $ Lex.symbol "=>"

  HLIR.MkExprOn name args <$> parseExpression

parseSpawn :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseSpawn = localize $ do
  void $ Lex.reserved "spawn"
  HLIR.MkExprSpawn <$> parseExpression

parseRequire :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseRequire = localize $ do
  void $ Lex.reserved "require"

  HLIR.MkExprRequire <$> Lex.lexeme Lit.parseString

parseTerm :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseTerm =
  localize $ P.choice [
    P.try parseActor,
    parseAnonActor,
    parseSpawn,
    P.try parseFunction,
    parseLambda,
    parseLet,
    parseMut,
    parseTernary,
    parseLiteral,
    parseBlock,
    parseList,
    P.try parseUpdate,
    parseVariable
  ]

parseExpression :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseExpression = P.makeExprParser parseTerm table
  where 
    table = [
        [
          P.Postfix . Lex.makeUnaryOp $ do
            field <- P.char '.' *> Lex.nonLexedID <* Lex.scn
            args <- P.option [] $ Lex.parens (P.sepBy parseExpression Lex.comma)
            let var = HLIR.MkExprVariable (HLIR.MkAnnotation field Nothing)
            pure $ \e -> HLIR.MkExprApplication var (e:args),

          P.Postfix . Lex.makeUnaryOp $ do
            args <- Lex.parens (P.sepBy parseExpression Lex.comma)
            pure $ \e -> HLIR.MkExprApplication e args,

          P.Postfix . Lex.makeUnaryOp $ do
            void $ Lex.symbol "->"
            name <- Lex.identifier
            args <- Lex.parens (P.sepBy parseExpression Lex.comma)

            pure $ \e -> HLIR.MkExprSend e name args 
        ],
        [
          P.InfixL $ do
            void $ Lex.symbol "*"
            pure $ \a b -> HLIR.MkExprBinary "*" Nothing a b,
          P.InfixL $ do
            void $ Lex.symbol "/"
            pure $ \a b -> HLIR.MkExprBinary "/" Nothing a b
        ],
        [
          P.InfixL $ do
            void $ Lex.symbol "+"
            pure $ \a b -> HLIR.MkExprBinary "+" Nothing a b,
          P.InfixL $ do
            void $ Lex.symbol "-"
            pure $ \a b -> HLIR.MkExprBinary "-" Nothing a b
        ],
        [
          P.InfixN $ do
            void $ Lex.symbol "=="
            pure $ \a b -> HLIR.MkExprBinary "==" Nothing a b,
          P.InfixN $ do
            void $ Lex.symbol "!="
            pure $ \a b -> HLIR.MkExprBinary "!=" Nothing a b
        ],
        [
          P.InfixN $ do
            void $ Lex.symbol ">"
            pure $ \a b -> HLIR.MkExprBinary ">" Nothing a b,
          P.InfixN $ do
            void $ Lex.symbol "<"
            pure $ \a b -> HLIR.MkExprBinary "<" Nothing a b,
          P.InfixN $ do
            void $ Lex.symbol ">="
            pure $ \a b -> HLIR.MkExprBinary ">=" Nothing a b,
          P.InfixN $ do
            void $ Lex.symbol "<="
            pure $ \a b -> HLIR.MkExprBinary "<=" Nothing a b
        ],
        [
          P.InfixL $ do
            void $ Lex.symbol "&&"
            pure $ \a b -> HLIR.MkExprBinary "&&" Nothing a b,
          P.InfixL $ do
            void $ Lex.symbol "||"
            pure $ \a b -> HLIR.MkExprBinary "||" Nothing a b
        ]
      ]

parseToplevel :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseToplevel =
  localize $ P.choice [
    parseModule,
    parseInterface,
    parseRequire,
    parseExtern,
    parseExpression
  ]

parseProgram :: MonadIO m => P.Parser m [HLIR.HLIR "expression"]
parseProgram = P.many parseToplevel <* P.eof
