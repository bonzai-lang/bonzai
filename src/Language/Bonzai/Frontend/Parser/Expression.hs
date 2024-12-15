module Language.Bonzai.Frontend.Parser.Expression where

import qualified Language.Bonzai.Frontend.Parser as P
import qualified Language.Bonzai.Frontend.Parser.Lexer as Lex
import qualified Language.Bonzai.Frontend.Parser.Internal.Literal as Lit
import qualified Language.Bonzai.Syntax.HLIR as HLIR
import qualified Language.Bonzai.Frontend.Parser.Internal.Type as Typ
import qualified Control.Monad.Combinators.Expr as P
import qualified Text.Megaparsec.Char as P
import qualified Data.Text as Text

-- | PARSE ANNOTATION
-- | Parse an annotation. An annotation is used to attach metadata to an AST node.
-- | In this context, an annotation is an identifier followed by an optional type, 
-- | consisting of the following:
-- |
-- | - name (":" type)?
parseAnnotation :: MonadIO m => P.Parser m a -> P.Parser m (HLIR.Annotation (Maybe a))
parseAnnotation p = P.choice [
    P.try $ do
      name <- Lex.identifier
      ty <- P.optional (Lex.symbol ":" *> p)

      pure $ HLIR.MkAnnotation name ty,
    HLIR.MkAnnotation <$> Lex.identifier <*> pure Nothing
  ]

-- | PARSE ANNOTATION'
-- | Parse an annotation. An annotation is used to attach metadata to an AST node.
-- | In this context, an annotation is an identifier followed by a type,
-- | consisting of the following:
-- |
-- | - name ":" type
parseAnnotation' :: MonadIO m => P.Parser m a -> P.Parser m (HLIR.Annotation a)
parseAnnotation' p = HLIR.MkAnnotation <$> Lex.identifier <*> (Lex.symbol ":" *> p)

-- | LOCALIZE
-- | Localize an AST node by attaching a position to it.
-- | This is used to attach a position to an AST node after it has been parsed.
-- | It makes use of the Locate typeclass to attach a position to an AST node 
-- | programmatically.
localize :: (MonadIO m, HLIR.Locate a) => P.Parser m a -> P.Parser m a
localize p = do
  startP <- P.getSourcePos
  x <- p
  endP <- P.getSourcePos

  pure $ HLIR.locate x (startP, endP)

-- | PARSE LITERAL
-- | Parsing a literal is just parsing a literal value except string literal, which
-- | is covered by the parseInterpolatedString function (used to parse interpolated
-- | strings).
parseLiteral :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseLiteral = localize . Lex.lexeme $ P.choice [
    HLIR.MkExprLiteral <$> Lit.parseLiteral,
    parseInterpolatedString
  ]

-- | PARSE INTERPOLATED STRING
-- | Parse an interpolated string. An interpolated string is a string that contains
-- | variables that are interpolated into the string. For instance, the string
-- | "Hello, $name" is an interpolated string that contains a variable called name.
-- | The variable name is interpolated into the string when the string is evaluated.
-- |
-- | To use the dollar sign anyway, you can escape it by using two followed dollar
-- | signs. For instance, the string "Hello, $$name" will be evaluated to "Hello, $name".
parseInterpolatedString :: (MonadIO m) => P.Parser m (HLIR.HLIR "expression")
parseInterpolatedString = removeEmptyStrings . buildString . toString <$> Lit.parseString
  where
    toString' :: HLIR.HLIR "expression" -> HLIR.HLIR "expression"
    toString' x = HLIR.MkExprApplication (HLIR.MkExprVariable (HLIR.MkAnnotation "toString" Nothing)) [x] Nothing

    buildString :: [Char] -> HLIR.HLIR "expression"
    buildString [] = HLIR.MkExprLiteral (HLIR.MkLitString "")
    buildString ('$':'$':xs) = HLIR.MkExprBinary "+" (HLIR.MkExprString "$") (buildString xs)
    buildString ('$':x:xs) | Lex.isIdentCharStart (Text.singleton x) = do
      -- Partition the string into a variable and the rest of the string
      let (var, rest) = span Lex.isIdentChar xs
      let var' = toString' $ HLIR.MkExprVariable (HLIR.MkAnnotation (Text.pack (x:var)) Nothing)

      HLIR.MkExprBinary "+" var' (buildString rest)
    buildString (x:xs) = do
      -- Partition the string into a string and the rest of the string
      let (str, rest) = span (/= '$') xs
      let str' = HLIR.MkExprLiteral (HLIR.MkLitString (Text.pack (x:str)))

      HLIR.MkExprBinary "+" str' (buildString rest)

    removeEmptyStrings :: HLIR.HLIR "expression" -> HLIR.HLIR "expression"
    removeEmptyStrings (HLIR.MkExprBinary "+" (HLIR.MkExprString "") x) = removeEmptyStrings x
    removeEmptyStrings (HLIR.MkExprBinary "+" x (HLIR.MkExprString "")) = removeEmptyStrings x
    removeEmptyStrings (HLIR.MkExprBinary "+" x y) = HLIR.MkExprBinary "+" (removeEmptyStrings x) (removeEmptyStrings y)
    removeEmptyStrings x = x

-- | PARSE TERNARY
-- | Parse a ternary expression. A ternary expression is an expression that consists
-- | of three parts: a condition, a then branch, and an else branch. It is used to
-- | conditionally evaluate an expression based on a condition.
-- | The syntax of a ternary expression is as follows:
-- |
-- | "if" expression "then" expression "else" expression
parseTernary :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseTernary = localize $ do
  void $ Lex.reserved "if"
  cond <- parseExpression

  void $ Lex.reserved "then"
  then' <- parseExpression

  void $ Lex.reserved "else"

  HLIR.MkExprTernary cond then' <$> parseExpression <*> pure Nothing

-- | PARSE LIST
-- | Parse a list expression. A list expression is an expression that consists of
-- | a list of expressions. It is used to represent a list of values in Bonzai.
-- | The syntax of a list expression is as follows:
-- |
-- | "[" expr ("," expr)* "]"
parseList :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseList = localize $ do
  void $ Lex.symbol "["
  exprs <- P.sepBy parseExpression Lex.comma
  void $ Lex.symbol "]"

  pure $ HLIR.MkExprList exprs

-- | PARSE EXTERN
-- | Parse an extern function. An extern function is a function that is defined 
-- | outside of the current module. It is used to define a function that is
-- | implemented in another language, externally to Bonzai.
-- | The syntax of an extern function is as follows:
-- |
-- | "extern" "fn" name "<" genrics ">" "(" arguments ")" (":" ret)?
parseExtern :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseExtern = localize $ do
  void $ Lex.reserved "extern"
  void $ Lex.reserved "fn"
  name <- Lex.identifier <|> Lex.parens Lex.operator
  gens <- P.option [] $ Lex.angles (P.sepBy Lex.identifier Lex.comma)

  args <- Lex.parens $ P.sepBy (parseAnnotation' Typ.parseType) Lex.comma
  ret <- P.option HLIR.MkTyUnit $ Lex.symbol ":" *> Typ.parseType

  let funTy = map (.value) args HLIR.:->: ret

  pure $ HLIR.MkExprNative (HLIR.MkAnnotation name gens) funTy

-- | PARSE PUBLIC
-- | Parse a public function. A public function is a function that is exposed to
-- | the outside world. It is used to define a function that is accessible from
-- | other modules.
-- | The syntax of a public function is as follows:
-- |
-- | "pub" toplevel
parsePublic :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parsePublic = localize $ do
  void $ Lex.reserved "pub"
  HLIR.MkExprPublic <$> parseToplevel

-- | PARSE VARIABLE
-- | Parse a variable expression. A variable expression is an expression that
-- | consists of a variable name. It is used to represent a variable in Bonzai.
-- | The syntax of a variable expression is as follows:
-- |
-- | identifier
parseVariable :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseVariable = localize $ do
  name <- Lex.lexeme Lex.identifier

  pure $ HLIR.MkExprVariable $ HLIR.MkAnnotation name Nothing

-- | PARSE LET DECLARATION
-- | Parse a let declaration. A let declaration is used to declare a variable
-- | and assign a value to it. It is used to bind a value to a variable in Bonzai.
-- | The syntax of a let declaration is as follows:
-- |
-- | "let" identifier "=" expression
parseLet :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseLet = localize $ do
  void $ Lex.reserved "let"
  name <- Lex.identifier <|> Lex.parens Lex.operator
  void $ Lex.reserved "="

  HLIR.MkExprLet mempty (HLIR.MkAnnotation name Nothing) <$> parseExpression

-- | PARSE LIVE DECLARATION
-- | Parse a live declaration. A live declaration is used to declare a live value
-- | in Bonzai. It is used to declare a value that is reactive about mutations.
-- | The syntax of a live declaration is as follows:
-- |
-- | "live" identifier "=" expression
parseLive :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseLive = localize $ do
  void $ Lex.reserved "live"
  name <- Lex.identifier <|> Lex.parens Lex.operator
  void $ Lex.reserved "="

  HLIR.MkExprLive (HLIR.MkAnnotation name Nothing) <$> parseExpression

-- | PARSE MUTABLE DECLARATION
-- | Parse a mutable declaration. A mutable declaration is used to declare a mutable
-- | value in Bonzai. It is used to declare a value that can be mutated.
-- | The syntax of a mutable declaration is as follows:
-- |
-- | "mut" identifier "=" expression
parseMut :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseMut = localize $ do
  void $ Lex.reserved "mut"
  name <- Lex.identifier <|> Lex.parens Lex.operator
  void $ Lex.reserved "="

  HLIR.MkExprLet mempty (HLIR.MkAnnotation name Nothing) . (`HLIR.MkExprMut` Nothing) <$> parseExpression

-- | PARSE MUTABLE EXPRESSION
-- | Parse a mutable expression. A mutable expression is an expression that consists
-- | of a mutable value. It is used to represent a value that can be mutated.
-- | The syntax of a mutable expression is as follows:
-- |
-- | "mut" expression
parseMutExpr :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseMutExpr = localize $ do
  void $ Lex.reserved "mut"

  HLIR.MkExprMut <$> parseExpression <*> pure Nothing

-- | PARSE DIRECT DATA
-- | Parse a direct data expression. A direct data expression is an expression that
-- | consists of a data type definition. It is used to define a data type in Bonzai
-- | that is not a sum type.
-- | The syntax of a direct data expression is as follows:
-- |
-- | "type" identifier ("<" generics ">")? ("(" arguments ")")?
parseDirectData :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseDirectData = localize $ do
  void $ Lex.reserved "type"
  name <- Lex.identifier
  gens <- P.option [] $ Lex.angles (P.sepBy Lex.identifier Lex.comma)

  params <- P.option [] $ Lex.parens (P.sepBy (parseAnnotation' Typ.parseType) Lex.comma)

  pure $ HLIR.MkExprData
    (HLIR.MkAnnotation name gens)
    [HLIR.MkDataConstructor name (map (.value) params)]

-- | PARSE DATA
-- | Parse a data expression. A data expression is an expression that consists of
-- | a sum type definition. It is used to define a data type in Bonzai that is a
-- | sum type.
-- | The syntax of a data expression is as follows:
-- |
-- | "type" identifier ("<" generics ">")? "{" (constructor | variable) "}"
parseDatatype :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseDatatype = localize $ do
  void $ Lex.reserved "type"
  name <- Lex.identifier
  gens <- P.option [] $ Lex.angles (P.sepBy Lex.identifier Lex.comma)

  HLIR.MkExprData (HLIR.MkAnnotation name gens) <$> Lex.braces (P.sepBy1 parseDataConstructor Lex.comma)

  where
    -- | PARSE DATA CONSTRUCTOR
    -- | Parse a data constructor. A data constructor is used to define a constructor
    -- | for a sum type in Bonzai. It is used to define a constructor for a sum type.
    -- | The syntax of a data constructor is as follows:
    -- |
    -- | - identifier "(" (annotation ",")* ")"
    -- | - identifier
    parseDataConstructor :: MonadIO m => P.Parser m (HLIR.DataConstructor HLIR.Type)
    parseDataConstructor = P.choice [
       P.try $ HLIR.MkDataConstructor
            <$> Lex.identifier
            <*> Lex.parens (P.sepBy ((.value) <$> parseAnnotation' Typ.parseType) Lex.comma),
        HLIR.MkDataVariable <$> Lex.identifier
      ]

-- | PARSE INTERFACE
-- | Parse an interface expression. An interface expression is an expression that
-- | consists of an interface definition. It is used to define actor behavior in
-- | Bonzai.
-- | The syntax of an interface expression is as follows:
-- |
-- | "interface" identifier ("<" generics ">")? "{" 
-- |   ("fn" identifier ("<" generics ">") "(" (annotation ",")* ")")*
-- | "}"
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
      name <- Lex.identifier <|> Lex.parens Lex.operator

      args <- P.option [] $ Lex.parens (P.sepBy (parseAnnotation' Typ.parseType) Lex.comma)

      let funTy = map (.value) args HLIR.:->: HLIR.MkTyUnit

      pure $ HLIR.MkAnnotation name funTy

-- | PARSE MATCH EXPRESSION
-- | Parse a match expression. A match expression is an expression that consists of
-- | a match statement. It is used to match a value against a set of patterns.
-- | The syntax of a match expression is as follows:
-- |
-- | "match" expression "{"
-- |   ("case" pattern "=>" expression)* 
-- | "}"
parseMatch :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseMatch = localize $ do
  void $ Lex.reserved "match"
  expr <- parseExpression

  void $ Lex.symbol "{"
  cases <- P.some parseCase
  void $ Lex.symbol "}"

  pure $ HLIR.MkExprMatch expr Nothing cases Nothing

  where
    parseCase :: MonadIO m => P.Parser m (HLIR.HLIR "pattern", HLIR.HLIR "expression", HLIR.Position)
    parseCase = do
      start <- P.getSourcePos
      void $ Lex.reserved "case"
      pat <- parsePattern

      void $ Lex.symbol "=>"
      expr <- parseExpression

      end <- P.getSourcePos

      pure (pat, expr, (start, end))

    -- | PARSE PATTERN
    -- | Parse a pattern. A pattern is used to match a value against a set of patterns.
    -- | It is used to match a value against a set of patterns in Bonzai.
    -- | The syntax of a pattern is as follows:
    -- |
    -- | - identifier
    -- | - literal
    -- | - "_" : wildcard
    -- | - "[" pattern ("," pattern)* "]"
    -- | - identifier "(" pattern ("," pattern)* ")"
    -- | - "[" pattern ".." pattern "]"
    parsePatternTerm :: MonadIO m => P.Parser m (HLIR.HLIR "pattern")
    parsePatternTerm = localize $ P.choice [
        Lex.brackets $ do
          pats <- P.sepBy parsePattern Lex.comma
          slice <- P.optional (localize $ Lex.symbol ".." *> parsePattern)

          pure (HLIR.MkPatList pats slice Nothing),
        P.try $ HLIR.MkPatConstructor <$> Lex.identifier <*> Lex.parens (P.sepBy1 parsePattern Lex.comma),
        HLIR.MkPatLiteral <$> Lex.lexeme (P.choice [Lit.parseLiteral, HLIR.MkLitString <$> Lit.parseString]),
        HLIR.MkPatWildcard <$ Lex.symbol "_",
        HLIR.MkPatVariable <$> Lex.identifier <*> pure Nothing
      ]

    -- | PARSE PATTERN
    -- | Parse a pattern. A pattern is used to match a value against a set of patterns.
    -- | It is used to match a value against a set of patterns in Bonzai.
    -- | The syntax of a pattern is as follows:
    -- |
    -- | - pattern_term
    -- | - pattern "|" pattern
    parsePattern :: MonadIO m => P.Parser m (HLIR.HLIR "pattern")
    parsePattern = localize $ P.makeExprParser parsePatternTerm table
      where
        table = [
            [
              P.InfixL $ do
                void $ Lex.symbol "|"
                pure $ \a b -> HLIR.MkPatOr a b
            ]
          ]

-- | PARSE TUPLE EXPRESSION
-- | Parse a tuple expression. A tuple expression is an expression that consists of
-- | a tuple of values. It is used to represent a fixed-size collection of values
-- | of different types in Bonzai.
-- | The syntax of a tuple expression is as follows:
-- |
-- | "(" expression "," expression ")"
parseTuple :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseTuple = localize . Lex.parens $ do
  x <- parseExpression
  void $ Lex.symbol ","
  HLIR.MkExprTuple x <$> parseExpression

-- | PARSE WHILE EXPRESSION
-- | Parse a while expression. A while expression is an expression that consists of
-- | a while loop. It is used to loop over a block of code while a condition is true.
-- | The syntax of a while expression is as follows:
-- |
-- | "while" expression "{" expression* "}"
parseWhile :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseWhile = localize $ do
  void $ Lex.reserved "while"
  cond <- parseExpression

  void $ Lex.symbol "{"
  body <- P.many parseExpression
  void $ Lex.symbol "}"

  pure $ HLIR.MkExprWhile cond (HLIR.MkExprBlock body Nothing)

-- | PARSE BLOCK EXPRESSION
-- | Parse a block expression. A block expression is an expression that consists of
-- | a block of code. It is used to group a set of expressions together in Bonzai.
-- | The syntax of a block expression is as follows:
-- |
-- | "{" expression* "}"
parseBlock :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseBlock = localize $ do
  void $ Lex.symbol "{"
  exprs <- P.many parseExpression
  void $ Lex.symbol "}"

  pure $ HLIR.MkExprBlock exprs Nothing

-- | PARSE FUNCTION EXPRESSION
-- | Parse a function expression. A function expression is an expression that consists
-- | of a function definition. It is used to define a function in Bonzai.
-- | The syntax of a function expression is as follows:
-- |
-- | "fn" identifier ("<" generics ">")? "(" arguments ")" (":" ret)? "=>" expression
parseFunction :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseFunction = localize $ do
  void $ Lex.reserved "fn"
  name <- Lex.identifier <|> Lex.parens Lex.operator
  generics <- P.option [] $ Lex.angles (P.sepBy Lex.identifier Lex.comma)
  args <- Lex.parens (P.sepBy (parseAnnotation Typ.parseType) Lex.comma)
  ret <- P.option Nothing $ Just <$> (Lex.symbol ":" *> Typ.parseType)

  void $ Lex.symbol "=>"

  let funTy = (HLIR.:->:) <$> mapM HLIR.value args <*> ret

  HLIR.MkExprLet (fromList generics) (HLIR.MkAnnotation name funTy) . HLIR.MkExprLambda args ret <$> parseExpression

-- | PARSE LAMBDA EXPRESSION
-- | Parse a lambda expression. A lambda expression is an expression that consists
-- | of a lambda function definition. It is used to define an anonymous function in
-- | Bonzai.
-- | The syntax of a lambda expression is as follows:
-- |
-- | "fn" "(" arguments ")" (":" ret)? "=>" expression
parseLambda :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseLambda = localize $ do
  void $ Lex.reserved "fn"
  args <- Lex.parens (P.sepBy (parseAnnotation Typ.parseType) Lex.comma)
  ret <- P.option Nothing $ Just <$> (Lex.symbol ":" *> Typ.parseType)

  void $ Lex.symbol "=>"

  HLIR.MkExprLambda args ret <$> parseExpression

-- | PARSE UPDATE EXPRESSION
-- | Parse an update expression. An update expression is an expression that consists
-- | of an update statement. It is used to update a value in Bonzai.
-- | The syntax of an update expression is as follows:
-- |
-- | identifier "=" expression
parseUpdate :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseUpdate = localize $ do
  name <- P.try $ (Lex.identifier <|> Lex.parens Lex.operator) <* Lex.symbol "="

  HLIR.MkExprUpdate (HLIR.MkUpdtVariable (HLIR.MkAnnotation name Nothing)) <$> parseExpression

-- | PARSE ACTOR EXPRESSION
-- | Parse an actor expression. An actor expression is an expression that consists
-- | of an actor definition. It is used to define an actor in Bonzai.
-- | The syntax of an actor expression is as follows:
-- |
-- | "actor" identifier ("<" type) "{" event* "}"
parseActor :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseActor = localize $ do
  void $ Lex.reserved "actor"
  name <- Lex.identifier <|> Lex.parens Lex.operator
  implemented <- Lex.symbol "<" *> Typ.parseType
  HLIR.MkExprLet mempty (HLIR.MkAnnotation name Nothing)
    . HLIR.MkExprActor implemented
      <$> Lex.braces (P.many parseEvent)

-- | PARSE ANONYMOUS ACTOR EXPRESSION
-- | Parse an anonymous actor expression. An anonymous actor expression is an expression
-- | that consists of an anonymous actor definition. It is used to define an actor
-- | without a name in Bonzai.
-- | The syntax of an anonymous actor expression is as follows:
-- |
-- | "actor" "<" type "{" event* "}"
parseAnonActor :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseAnonActor = localize $ do
  void $ Lex.reserved "actor"
  implemented <- Lex.symbol "<" *> Typ.parseType
  HLIR.MkExprActor implemented <$> Lex.braces (P.many parseEvent)

-- | PARSE EVENT
-- | Parse an event expression. An event expression is an expression that consists
-- | of an event definition. It is used to define an event in Bonzai.
-- | The syntax of an event expression is as follows:
-- |
-- | "on" identifier "(" arguments ")" "=>" expression
parseEvent :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseEvent = localize $ do
  void $ Lex.reserved "on"
  name <- Lex.identifier <|> Lex.parens Lex.operator
  args <- Lex.parens (P.sepBy (parseAnnotation Typ.parseType) Lex.comma)

  void $ Lex.symbol "=>"

  HLIR.MkExprOn name args <$> parseExpression

-- | PARSE SPAWN
-- | Parse a spawn expression. A spawn expression is an expression that consists of
-- | a spawn statement. It is used to spawn a new actor in Bonzai.
-- | The syntax of a spawn expression is as follows:
-- |
-- | "spawn" expression
parseSpawn :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseSpawn = localize $ do
  void $ Lex.reserved "spawn"
  HLIR.MkExprSpawn <$> parseExpression

-- | PARSE MAP
-- | Parse a map expression. A map expression is an expression that consists of a map
-- | definition. It is used to define a map in Bonzai.
-- | The syntax of a map expression is as follows:
-- |
-- | "#{" (key ":" value ("," key ":" value)*)? "}"
parseMap :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseMap = do
  xs <- P.string "#{" *> Lex.scn *> P.sepBy parseMapPair Lex.comma <* Lex.symbol "}"

  let mapVar = HLIR.MkExprVariable (HLIR.MkAnnotation "Map" Nothing)
  pure $ HLIR.MkExprApplication mapVar [HLIR.MkExprList xs] Nothing

  where
    parseMapPair :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
    parseMapPair = do
      key <- P.choice [
          HLIR.MkExprLiteral . HLIR.MkLitString <$> Lex.identifier,
          parseInterpolatedString
        ]
      void $ Lex.symbol ":"

      HLIR.MkExprTuple key <$> parseExpression

-- | PARSE REQUIRE
-- | Parse a require expression. A require expression is an expression that consists
-- | of a require statement. It is used to import a module in Bonzai.
-- | The syntax of a require expression is as follows:
-- |
-- | "require" string (":" identifier ("," identifier)*)?
parseRequire :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseRequire = localize $ do
  void $ Lex.reserved "require"

  path <- Lex.lexeme Lit.parseString

  vars <- P.option [] $ Lex.symbol ":" *> P.sepBy1 Lex.identifier Lex.comma

  pure $ HLIR.MkExprRequire path (fromList vars)

-- | PARSE TRY-CATCH EXPRESSION
-- | Parse a try-catch expression. A try-catch expression is an expression that consists
-- | of a try-catch statement. It is used to catch an error in Bonzai.
-- | The syntax of a try-catch expression is as follows:
-- |
-- | "try" expression "catch" identifier "{" expression* "}"
parseTryCatch :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseTryCatch = localize $ do
  void $ Lex.reserved "try"
  expr <- parseExpression

  void $ Lex.reserved "catch"
  name <- Lex.identifier <|> Lex.parens Lex.operator

  void $ Lex.symbol "{"
  body <- P.many parseExpression
  void $ Lex.symbol "}"

  pure $ HLIR.MkExprTryCatch expr (HLIR.MkAnnotation name Nothing) (HLIR.MkExprBlock body Nothing)

-- | PARSE TERM EXPRESSION
-- | Parse a term expression. A term expression is an expression that consists of a term.
-- | It is used to represent a non-recursive value in Bonzai.
parseTerm :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseTerm =
  localize $ P.choice [
    parseWhile,
    P.try parseActor,
    parseAnonActor,
    parseSpawn,
    P.try parseFunction,
    parseLambda,
    parseLet,
    parseLive,
    P.try parseMut,
    parseMutExpr,
    parseMatch,
    parseTernary,
    parseLiteral,
    P.try parseMap,
    parseBlock,
    parseList,
    parseTryCatch,
    P.try parseTuple,
    P.try parseUpdate,
    parseVariable,
    Lex.parens parseExpression
  ]

-- | PARSE EXPRESSION
-- | Parse an expression. An expression is a piece of code that can be evaluated to
-- | a value. It is used to represent a value in Bonzai.
-- | The syntax of an expression is as follows:
-- |
-- | - term
-- | - term ("." term)*
-- | - term "[" expression "]"
-- | - term "->" identifier "(" (expression ",")* ")"
-- | - term "*" term
-- | - term "/" term
-- | - term "+" term
-- | - term "-" term
-- | - term "==" term
-- | - term "!=" term
-- | - term ">=" term
-- | - term "<=" term
-- | - term ">" term
-- | - term "<" term
-- | - term "&&" term
-- | - term "||" term
-- | - "!" term
-- | - term operator term
parseExpression :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseExpression = localize $ P.makeExprParser parseTerm table
  where
    table = [
        [
          P.Postfix . Lex.makeUnaryOp $ do
            args <- Lex.parens (P.sepBy parseExpression Lex.comma)
            pure $ \e -> HLIR.MkExprApplication e args Nothing
        ],
        [
            P.Postfix . Lex.makeUnaryOp $ do
              field <- P.char '.' *> Lex.nonLexedID <* Lex.scn
              args <- P.option [] $ Lex.parens (P.sepBy parseExpression Lex.comma)
              let var = HLIR.MkExprVariable (HLIR.MkAnnotation field Nothing)
              pure $ \e -> HLIR.MkExprApplication var (e:args) Nothing
        ],
        [
          P.Postfix . Lex.makeUnaryOp $ do
            void $ Lex.symbol "["
            idx <- parseExpression
            void $ Lex.symbol "]"

            pure $ \e -> HLIR.MkExprIndex e idx
        ],
        [
          P.Postfix . Lex.makeUnaryOp $ do
            void $ Lex.symbol "->"
            name <- Lex.identifier
            args <- Lex.parens (P.sepBy parseExpression Lex.comma)

            pure $ \e -> HLIR.MkExprSend e name args Nothing
        ],
        [
          P.InfixL $ do
            void $ Lex.symbol "*"
            pure $ \a b -> HLIR.MkExprBinary "*" a b,
          P.InfixL $ do
            void $ Lex.symbol "/"
            pure $ \a b -> HLIR.MkExprBinary "/" a b
        ],
        [
          P.InfixL $ do
            void $ Lex.symbol "+"
            pure $ \a b -> HLIR.MkExprBinary "+" a b,
          P.InfixL $ do
            void $ Lex.symbol "-"
            pure $ \a b -> HLIR.MkExprBinary "-" a b
        ],
        [
          P.InfixN $ do
            void $ Lex.symbol "=="
            pure $ \a b -> HLIR.MkExprBinary "==" a b,
          P.InfixN $ do
            void $ Lex.symbol "!="
            pure $ \a b -> HLIR.MkExprBinary "!=" a b
        ],
        [
          P.InfixN $ do
            void $ Lex.symbol ">="
            pure $ \a b -> HLIR.MkExprBinary ">=" a b,
          P.InfixN $ do
            void $ Lex.symbol "<="
            pure $ \a b -> HLIR.MkExprBinary "<=" a b,
          P.InfixN $ do
            void $ Lex.symbol ">"
            pure $ \a b -> HLIR.MkExprBinary ">" a b,
          P.InfixN $ do
            void $ Lex.symbol "<"
            pure $ \a b -> HLIR.MkExprBinary "<" a b
        ],
        [
          P.InfixL $ do
            void $ Lex.symbol "&&"
            pure $ \a b -> HLIR.MkExprBinary "&&" a b,
          P.InfixL $ do
            void $ Lex.symbol "||"
            pure $ \a b -> HLIR.MkExprBinary "||" a b
        ],
        [
          P.Prefix . Lex.makeUnaryOp $ do
            void $ Lex.symbol "!"
            pure $ \a -> HLIR.MkExprApplication (HLIR.MkExprVariable (HLIR.MkAnnotation "!" Nothing)) [a] Nothing
        ],
        [
          P.InfixL $ do
            op <- Lex.operator
            pure $ \a b -> HLIR.MkExprBinary op a b
        ],
        [
          P.InfixL $ do
            Lex.scn
            void $ P.char ':'
            name <- Lex.nonLexedID
            void $ P.char ':'
            Lex.scn

            pure $ \a b -> HLIR.MkExprBinary name a b
        ]
      ]

-- | PARSE TOPLEVEL
-- | Parse a toplevel expression. A toplevel expression is an expression that is
-- | at the top level of a module. It is used to define a module in Bonzai.
parseToplevel :: MonadIO m => P.Parser m (HLIR.HLIR "expression")
parseToplevel =
  localize $ P.choice [
    parsePublic,
    parseInterface,
    P.try parseDatatype,
    parseDirectData,
    parseRequire,
    parseExtern,
    parseExpression
  ]

-- | PARSE PROGRAM
-- | Parse a program. A program is a list of toplevel expressions. It is used to
-- | define a module in Bonzai.
parseProgram :: MonadIO m => P.Parser m [HLIR.HLIR "expression"]
parseProgram = Lex.scn *> P.many parseToplevel <* P.eof
