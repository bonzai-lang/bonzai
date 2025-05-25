module Language.Bonzai.Frontend.Parser.Expression where

import Control.Monad.Combinators.Expr qualified as P
import Data.Foldable qualified as List
import Data.Text qualified as Text
import GHC.IO qualified as IO
import Language.Bonzai.Frontend.Parser qualified as P
import Language.Bonzai.Frontend.Parser.Internal.Literal qualified as Lit
import Language.Bonzai.Frontend.Parser.Internal.Type qualified as Typ
import Language.Bonzai.Frontend.Parser.Lexer qualified as Lex
import Language.Bonzai.Frontend.Typechecking.Monad qualified as M
import Language.Bonzai.Syntax.HLIR qualified as HLIR
import Text.Megaparsec.Char qualified as P

-- | PARSE ANNOTATION
-- | Parse an annotation. An annotation is used to attach metadata to an AST node.
-- | In this context, an annotation is an identifier followed by an optional type,
-- | consisting of the following:
-- |
-- | - name (":" type)?
parseAnnotation :: (MonadIO m) => P.Parser m a -> P.Parser m (HLIR.Annotation (Maybe a))
parseAnnotation p =
  P.choice
    [ P.try $ do
        name <- snd <$> Lex.identifier
        ty <- P.optional (Lex.symbol ":" *> p)

        pure $ HLIR.MkAnnotation name ty,
      HLIR.MkAnnotation . snd <$> Lex.identifier <*> pure Nothing
    ]

-- | PARSE ANNOTATION'
-- | Parse an annotation. An annotation is used to attach metadata to an AST node.
-- | In this context, an annotation is an identifier followed by a type,
-- | consisting of the following:
-- |
-- | - name ":" type
parseAnnotation' :: (MonadIO m) => P.Parser m a -> P.Parser m (HLIR.Annotation a)
parseAnnotation' p = HLIR.MkAnnotation . snd <$> Lex.identifier <*> (Lex.symbol ":" *> p)

-- | PARSE LITERAL
-- | Parsing a literal is just parsing a literal value except string literal, which
-- | is covered by the parseInterpolatedString function (used to parse interpolated
-- | strings).
parseLiteral :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseLiteral =
  Lex.locateWith
    <$> Lex.lexeme
      ( P.choice
          [ HLIR.MkExprLiteral <$> Lit.parseLiteral,
            parseInterpolatedString
          ]
      )

-- | SYMBOL COUNTER
-- | Used to generate unique symbols for the parser, especially for the lambda
-- | parser.
symbolCounter :: IORef Int
symbolCounter = IO.unsafePerformIO $ newIORef 0

-- | FRESH SYMBOL
-- | Generate a fresh symbol for the parser.
freshSymbol :: (MonadIO m) => m Text
freshSymbol = do
  i <- atomicModifyIORef symbolCounter (\i -> (i + 1, i))

  pure $ Text.pack ("$symbol_" <> show i)

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
    toString' x = HLIR.MkExprApplication (HLIR.MkExprVariable (HLIR.MkAnnotation "toString" Nothing)) [x, HLIR.MkExprRecordEmpty]

    buildString :: [Char] -> HLIR.HLIR "expression"
    buildString [] = HLIR.MkExprLiteral (HLIR.MkLitString "")
    buildString ('$' : '$' : xs) = HLIR.MkExprBinary "+" (HLIR.MkExprString "$") (buildString xs)
    buildString ('$' : x : xs) | Lex.isIdentCharStart (Text.singleton x) = do
      -- Partition the string into a variable and the rest of the string
      let (var, rest) = span Lex.isIdentChar xs
      let var' = toString' $ HLIR.MkExprVariable (HLIR.MkAnnotation (Text.pack (x : var)) Nothing)

      HLIR.MkExprBinary "+" var' (buildString rest)
    buildString (x : xs) = do
      -- Partition the string into a string and the rest of the string
      let (str, rest) = span (/= '$') xs
      let str' = HLIR.MkExprLiteral (HLIR.MkLitString (Text.pack (x : str)))

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
parseTernary :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseTernary = do
  ((start, _), _) <- Lex.reserved "if"
  (_, cond) <- parseExpression

  void $ Lex.reserved "then"
  ((_, end1), then') <- parseExpression

  else' <- P.optional $ void (Lex.reserved "else") *> parseExpression

  case else' of
    Just ((_, end2), else'') -> pure ((start, end2), HLIR.MkExprTernary cond then' else'')
    Nothing -> pure ((start, end1), HLIR.MkExprSingleIf cond then')

-- | PARSE LIST
-- | Parse a list expression. A list expression is an expression that consists of
-- | a list of expressions. It is used to represent a list of values in Bonzai.
-- | The syntax of a list expression is as follows:
-- |
-- | "[" expr ("," expr)* "]"
parseList :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseList = do
  ((start, _), _) <- Lex.symbol "["
  exprs <- map snd <$> P.sepBy parseExpression Lex.comma
  ((_, end), _) <- Lex.symbol "]"

  pure ((start, end), HLIR.MkExprList exprs)

-- | PARSE EXTERN
-- | Parse an extern function. An extern function is a function that is defined
-- | outside of the current module. It is used to define a function that is
-- | implemented in another language, externally to Bonzai.
-- | The syntax of an extern function is as follows:
-- |
-- | "extern" "fn" name "<" genrics ">" "(" arguments ")" (":" ret)?
parseExtern :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseExtern = do
  ((start, _), _) <- Lex.reserved "extern"
  void $ Lex.reserved "fn"
  (_, name) <- Lex.identifier <|> Lex.parens (snd <$> Lex.operator)
  gens <- P.option [] $ snd <$> Lex.angles (P.sepBy (snd <$> Lex.identifier) Lex.comma)

  args <- (snd <$>) . Lex.parens $ P.sepBy (parseAnnotation' (snd <$> Typ.parseType)) Lex.comma
  ((_, end), ret) <- Lex.symbol ":" *> Typ.parseType

  kwargs <- M.fresh

  let funTy = (map (.value) args ++ [kwargs]) HLIR.:->: ret

  pure ((start, end), HLIR.MkExprNative (HLIR.MkAnnotation name gens) funTy)

-- | PARSE PUBLIC
-- | Parse a public function. A public function is a function that is exposed to
-- | the outside world. It is used to define a function that is accessible from
-- | other modules.
-- | The syntax of a public function is as follows:
-- |
-- | "pub" toplevel
parsePublic :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parsePublic = do
  ((start, _), _) <- Lex.reserved "pub"
  ((_, end), expr) <- parseToplevel
  pure ((start, end), HLIR.MkExprPublic expr)

-- | PARSE VARIABLE
-- | Parse a variable expression. A variable expression is an expression that
-- | consists of a variable name. It is used to represent a variable in Bonzai.
-- | The syntax of a variable expression is as follows:
-- |
-- | identifier
parseVariable :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseVariable = do
  (pos, name) <- Lex.identifier

  pure (pos, HLIR.MkExprVariable $ HLIR.MkAnnotation name Nothing)

-- | PARSE LET DECLARATION
-- | Parse a let declaration. A let declaration is used to declare a variable
-- | and assign a value to it. It is used to bind a value to a variable in Bonzai.
-- | The syntax of a let declaration is as follows:
-- |
-- | "let" identifier "=" expression
parseLet :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseLet = do
  ((start, _), _) <- Lex.reserved "let"
  name <- (Right <$> P.try parsePattern) <|> (Left . snd <$> (Lex.identifier <|> Lex.parens (snd <$> Lex.operator)))
  void $ Lex.reserved "="
  ((_, end1), expr) <- parseExpression

  ((_, end2), body) <- P.option ((start, end1), HLIR.MkExprVariable (HLIR.MkAnnotation "unit" Nothing)) $ Lex.reserved "in" *> parseExpression

  pure
    ( (start, end2),
      HLIR.MkExprLet
        mempty
        ( case name of
            Left n -> Left (HLIR.MkAnnotation n Nothing)
            Right pat | isPatVar pat, Just name' <- getPatVar pat -> Left (HLIR.MkAnnotation name' Nothing)
            Right pat -> Right pat
        )
        expr
        body
    )

-- | PARSE MUTABLE DECLARATION
-- | Parse a mutable declaration. A mutable declaration is used to declare a mutable
-- | value in Bonzai. It is used to declare a value that can be mutated.
-- | The syntax of a mutable declaration is as follows:
-- |
-- | "mut" identifier "=" expression
parseMut :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseMut = do
  ((start, _), _) <- Lex.reserved "mut"
  name <- (Right <$> P.try parsePattern) <|> (Left . snd <$> (Lex.identifier <|> Lex.parens (snd <$> Lex.operator)))
  void $ Lex.reserved "="
  ((_, end1), expr) <- parseExpression

  (pos, body) <- P.option ((start, end1), HLIR.MkExprVariable (HLIR.MkAnnotation "unit" Nothing)) $ Lex.reserved "in" *> parseExpression

  pure
    ( pos,
      HLIR.MkExprLet
        mempty
        ( case name of
            Left n -> Left (HLIR.MkAnnotation n Nothing)
            Right pat | isPatVar pat, Just name' <- getPatVar pat -> Left (HLIR.MkAnnotation name' Nothing)
            Right pat -> Right pat
        )
        (HLIR.MkExprMut expr)
        body
    )

isPatVar :: HLIR.HLIR "pattern" -> Bool
isPatVar (HLIR.MkPatVariable _ _) = True
isPatVar (HLIR.MkPatLocated p _) = isPatVar p
isPatVar _ = False

getPatVar :: HLIR.HLIR "pattern" -> Maybe Text
getPatVar (HLIR.MkPatVariable n _) = Just n
getPatVar (HLIR.MkPatLocated p _) = getPatVar p
getPatVar _ = Nothing

-- | PARSE MUTABLE EXPRESSION
-- | Parse a mutable expression. A mutable expression is an expression that consists
-- | of a mutable value. It is used to represent a value that can be mutated.
-- | The syntax of a mutable expression is as follows:
-- |
-- | "mut" expression
parseMutExpr :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseMutExpr = do
  ((start, _), _) <- Lex.reserved "mut"
  ((_, end), expr) <- parseExpression
  pure ((start, end), HLIR.MkExprMut expr)

-- | PARSE DIRECT DATA
-- | Parse a direct data expression. A direct data expression is an expression that
-- | consists of a data type definition. It is used to define a data type in Bonzai
-- | that is not a sum type.
-- | The syntax of a direct data expression is as follows:
-- |
-- | "type" identifier ("<" generics ">")? ("(" arguments ")")?
parseDirectData :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseDirectData = do
  ((start, _), _) <- Lex.reserved "type"
  ((_, end'), name) <- Lex.identifier
  (pos, gens) <- P.option ((start, end'), []) $ Lex.angles (map snd <$> P.sepBy Lex.identifier Lex.comma)

  (pos', params) <- P.option (pos, []) $ Lex.parens (P.sepBy (parseAnnotation' (snd <$> Typ.parseType)) Lex.comma)

  kwarg <- M.fresh

  pure
    ( pos',
      HLIR.MkExprData
        (HLIR.MkAnnotation name gens)
        [HLIR.MkDataConstructor name (map (.value) params ++ [kwarg])]
    )

-- | PARSE DATA
-- | Parse a data expression. A data expression is an expression that consists of
-- | a sum type definition. It is used to define a data type in Bonzai that is a
-- | sum type.
-- | The syntax of a data expression is as follows:
-- |
-- | "type" identifier ("<" generics ">")? "{" (constructor | variable) "}"
parseDatatype :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseDatatype = do
  ((start, _), _) <- Lex.reserved "type"
  (_, name) <- Lex.identifier
  gens <- P.option [] $ snd <$> Lex.angles (map snd <$> P.sepBy Lex.identifier Lex.comma)

  ((_, end), constructors) <- Lex.braces (P.sepBy1 parseDataConstructor Lex.comma)

  pure ((start, end), HLIR.MkExprData (HLIR.MkAnnotation name gens) constructors)
  where
    -- \| PARSE DATA CONSTRUCTOR
    -- \| Parse a data constructor. A data constructor is used to define a constructor
    -- \| for a sum type in Bonzai. It is used to define a constructor for a sum type.
    -- \| The syntax of a data constructor is as follows:
    -- \|
    -- \| - identifier "(" (annotation ",")* ")"
    -- \| - identifier
    parseDataConstructor :: (MonadIO m) => P.Parser m (HLIR.DataConstructor HLIR.Type)
    parseDataConstructor =
      P.choice
        [ P.try $ do
            (_, name) <- Lex.identifier
            (_, args) <- Lex.parens (P.sepBy (parseAnnotation' (snd <$> Typ.parseType)) Lex.comma)

            kwarg <- M.fresh

            pure $ HLIR.MkDataConstructor name (map (.value) args ++ [kwarg]),
          HLIR.MkDataVariable . snd <$> Lex.identifier
        ]

-- | PARSE RECORD
-- | Parse a record expression. A record expression is an expression that consists
-- | of a record of values. It is used to represent a record of values in Bonzai.
parseExprRecord :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseExprRecord = do
  ((start, _), _) <- Lex.symbol "{"
  fields <- P.sepBy parseField Lex.comma
  rest <- P.optional $ do
    void $ Lex.symbol "|"
    snd <$> parseExpression
  ((_, end), _) <- Lex.symbol "}"

  pure
    ( (start, end),
      List.foldl
        (\acc (idt, ty) -> HLIR.MkExprRecordExtension acc idt False ty)
        (fromMaybe HLIR.MkExprRecordEmpty rest)
        fields
    )
  where
    parseField :: (MonadIO m) => P.Parser m (Text, HLIR.HLIR "expression")
    parseField = do
      (_, key) <- Lex.identifier
      void $ Lex.symbol ":"
      (_, value) <- parseExpression

      pure (key, value)

-- | PARSE MATCH EXPRESSION
-- | Parse a match expression. A match expression is an expression that consists of
-- | a match statement. It is used to match a value against a set of patterns.
-- | The syntax of a match expression is as follows:
-- |
-- | "match" expression "{"
-- |   ("case" pattern "=>" expression)*
-- | "}"
parseMatch :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseMatch = do
  ((start, _), _) <- Lex.reserved "match"
  (_, expr) <- parseExpression

  void $ Lex.symbol "{"
  cases <- P.some parseCase
  ((_, end), _) <- Lex.symbol "}"

  pure ((start, end), HLIR.MkExprMatch expr cases)
  where
    parseCase :: (MonadIO m) => P.Parser m (HLIR.HLIR "pattern", HLIR.HLIR "expression", Maybe HLIR.Position)
    parseCase = do
      start <- P.getSourcePos
      void $ Lex.reserved "case"
      pat <- parsePattern
      end <- P.getSourcePos

      void $ Lex.symbol "=>"
      (_, expr) <- parseExpression

      pure (pat, expr, Just (start, end))

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
parsePatternTerm :: (MonadIO m) => P.Parser m (HLIR.HLIR "pattern")
parsePatternTerm =
  P.choice
    [ do
        (pos, pat) <- Lex.brackets $ do
          pats <- P.sepBy parsePattern Lex.comma
          slice <- P.optional (Lex.symbol ".." *> parsePattern)

          pure (HLIR.MkPatList pats slice Nothing)

        pure $ HLIR.MkPatLocated pat pos,
      P.try $ do
        ((start, _), n) <- Lex.identifier
        void $ Lex.symbol "("
        pats <- P.sepBy parsePattern Lex.comma
        ((_, end), _) <- Lex.symbol ")"

        pure $ HLIR.MkPatLocated (HLIR.MkPatConstructor n pats) (start, end),
      P.try $ do
        ((start, _), _) <- Lex.symbol "("
        pats <- P.sepBy parsePattern Lex.comma
        ((_, end), _) <- Lex.symbol ")"

        pure $ HLIR.MkPatLocated (HLIR.MkPatList pats Nothing Nothing) (start, end),
      do
        (pos, lit) <- Lex.lexeme (P.choice [Lit.parseLiteral, HLIR.MkLitString <$> Lit.parseString])

        pure $ HLIR.MkPatLocated (HLIR.MkPatLiteral lit) pos,
      do
        (pos, _) <- Lex.symbol "_"
        pure $ HLIR.MkPatLocated HLIR.MkPatWildcard pos,
      do
        (pos, n) <- Lex.identifier
        pure $ HLIR.MkPatLocated (HLIR.MkPatVariable n Nothing) pos
    ]

-- | PARSE PATTERN
-- | Parse a pattern. A pattern is used to match a value against a set of patterns.
-- | It is used to match a value against a set of patterns in Bonzai.
-- | The syntax of a pattern is as follows:
-- |
-- | - pattern_term
-- | - pattern "|" pattern
parsePattern :: (MonadIO m) => P.Parser m (HLIR.HLIR "pattern")
parsePattern = P.makeExprParser parsePatternTerm table
  where
    table =
      [ [ P.InfixL $ do
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
parseTuple :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseTuple = do
  ((start, _), _) <- Lex.symbol "("
  (_, x) <- parseExpression
  void $ Lex.symbol ","
  (_, y) <- parseExpression
  ((_, end), _) <- Lex.symbol ")"
  pure ((start, end), HLIR.MkExprTuple x y)

-- | PARSE WHILE EXPRESSION
-- | Parse a while expression. A while expression is an expression that consists of
-- | a while loop. It is used to loop over a block of code while a condition is true.
-- | The syntax of a while expression is as follows:
-- |
-- | "while" expression "{" expression* "}"
parseWhile :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseWhile = do
  ((start, _), _) <- Lex.reserved "while"
  (_, cond) <- parseExpression

  void $ Lex.symbol "{"
  body <- map snd <$> P.sepEndBy parseStatement (P.optional (Lex.symbol ";"))
  ((_, end), _) <- Lex.symbol "}"

  pure ((start, end), HLIR.MkExprWhile cond (HLIR.MkExprBlock body))

-- | PARSE FOR-IN EXPRESSION
-- | Parse a for-in expression. A for-in expression is an expression that consists of
-- | a for loop. It is used to loop over a collection of values in Bonzai.
-- | The syntax of a for-in expression is as follows:
-- |
-- | "for" (identifier | pattern) "in" expression "{" expression* "}"
parseForIn :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseForIn = do
  ((start, _), _) <- Lex.reserved "for"
  name <- (Right <$> parsePattern) <|> (Left . snd <$> (Lex.identifier <|> Lex.parens (snd <$> Lex.operator)))
  let name' = case name of
        Left n -> Left (HLIR.MkAnnotation n Nothing)
        Right pat | isPatVar pat, Just name'' <- getPatVar pat -> Left (HLIR.MkAnnotation name'' Nothing)
        Right pat -> Right pat
  void $ Lex.reserved "in"
  (_, expr) <- parseExpression

  void $ Lex.symbol "{"
  body <- map snd <$> P.sepEndBy parseStatement (P.optional (Lex.symbol ";"))
  ((_, end), _) <- Lex.symbol "}"

  let mutable =
        HLIR.MkExprApplication
          (HLIR.MkExprVariable (HLIR.MkAnnotation "value" Nothing))
          [ HLIR.MkExprVariable (HLIR.MkAnnotation "@i" Nothing),
            HLIR.MkExprRecordEmpty
          ]
  let len =
        HLIR.MkExprApplication
          (HLIR.MkExprVariable (HLIR.MkAnnotation "List::length" Nothing))
          [ HLIR.MkExprVariable (HLIR.MkAnnotation "@array" Nothing),
            HLIR.MkExprRecordEmpty
          ]
  let cond = HLIR.MkExprBinary "<" mutable len

  pure
    ( (start, end),
      HLIR.MkExprBlock
        [ HLIR.MkExprLet
            mempty
            (Left (HLIR.MkAnnotation "@i" Nothing))
            (HLIR.MkExprMut (HLIR.MkExprLiteral (HLIR.MkLitInt 0)))
            (HLIR.MkExprVariable (HLIR.MkAnnotation "unit" Nothing)),
          HLIR.MkExprLet
            mempty
            (Left (HLIR.MkAnnotation "@array" Nothing))
            expr
            (HLIR.MkExprVariable (HLIR.MkAnnotation "unit" Nothing)),
          HLIR.MkExprWhile
            cond
            ( HLIR.MkExprBlock $
                [ HLIR.MkExprLet
                    mempty
                    name'
                    (HLIR.MkExprIndex (HLIR.MkExprVariable (HLIR.MkAnnotation "@array" Nothing)) mutable)
                    (HLIR.MkExprVariable (HLIR.MkAnnotation "unit" Nothing))
                ]
                  <> body
                  <> [ HLIR.MkExprMutableOperation "+=" (HLIR.MkExprVariable (HLIR.MkAnnotation "@i" Nothing)) (HLIR.MkExprLiteral (HLIR.MkLitInt 1))
                     ]
            ),
          HLIR.MkExprVariable (HLIR.MkAnnotation "unit" Nothing)
        ]
    )

-- | PARSE BLOCK EXPRESSION
-- | Parse a block expression. A block expression is an expression that consists of
-- | a block of code. It is used to group a set of expressions together in Bonzai.
-- | The syntax of a block expression is as follows:
-- |
-- | "{" expression* "}"
parseBlock :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseBlock = do
  ((start, _), _) <- Lex.symbol "{"
  exprs <- map snd <$> P.sepEndBy parseStatement (P.optional (Lex.symbol ";"))
  ((_, end), _) <- Lex.symbol "}"

  pure ((start, end), HLIR.MkExprBlock exprs)

argument ::
  (MonadIO m) =>
  P.Parser
    m
    ( Position
        (HLIR.HLIR "pattern")
        (HLIR.Annotation (Maybe HLIR.Type))
        (Bool, HLIR.Annotation (Maybe HLIR.Type))
    )
argument =
  P.choice
    [ P.try $ do
        name <- snd <$> Lex.identifier
        void $ Lex.symbol "_"
        opt <- P.option False $ Lex.symbol "?" $> True
        ty <- P.optional (Lex.symbol ":" *> (snd <$> Typ.parseType))

        pure (Middle' (opt, HLIR.MkAnnotation name ty)),
      P.try $ do
        name <- snd <$> Lex.identifier
        void $ Lex.symbol ":"
        Right' . HLIR.MkAnnotation name . Just . snd <$> Typ.parseType,
      Left' <$> parsePattern
    ]

data Position a b c
  = Left' a
  | Right' b
  | Middle' c
  deriving (Show, Eq)

lefts' :: [Position a b c] -> [a]
lefts' [] = []
lefts' (Left' x : xs) = x : lefts' xs
lefts' (Right' _ : xs) = lefts' xs
lefts' (Middle' _ : xs) = lefts' xs

rights' :: [Position a b c] -> [b]
rights' [] = []
rights' (Left' _ : xs) = rights' xs
rights' (Right' x : xs) = x : rights' xs
rights' (Middle' _ : xs) = rights' xs

middles' :: [Position a b c] -> [c]
middles' [] = []
middles' (Left' _ : xs) = middles' xs
middles' (Right' _ : xs) = middles' xs
middles' (Middle' x : xs) = x : middles' xs

-- | PARSE FUNCTION EXPRESSION
-- | Parse a function expression. A function expression is an expression that consists
-- | of a function definition. It is used to define a function in Bonzai.
-- | The syntax of a function expression is as follows:
-- |
-- | "fn" identifier ("<" generics ">")? "(" arguments ")" (":" ret)? "=>" expression
parseFunction :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseFunction = do
  ((start, _), _) <- Lex.reserved "fn"
  (_, name) <- Lex.identifier <|> Lex.parens (snd <$> Lex.operator)

  generics <- P.option [] $ snd <$> Lex.angles (map snd <$> P.sepBy Lex.identifier Lex.comma)
  args <- snd <$> Lex.parens (P.sepBy argument Lex.comma)
  ret <- P.option Nothing $ Just . snd <$> (Lex.symbol ":" *> Typ.parseType)

  void $ Lex.symbol "=>"

  let args' = (Left <$> lefts' args) <> (Right <$> rights' args)
  let labels = middles' args

  ((_, end), expr) <- parseExpression

  (args'', body') <-
    List.foldlM
      ( \(vars, acc) x -> case x of
          Right a -> pure (vars <> [a], acc)
          Left (HLIR.MkPatVariable name' _) ->
            pure (vars <> [HLIR.MkAnnotation name' Nothing], acc)
          Left (HLIR.MkPatLocated p _)
            | Just name' <- getPatVar p ->
                pure (vars <> [HLIR.MkAnnotation name' Nothing], acc)
          Left p -> do
            name' <- freshSymbol
            pure (vars <> [HLIR.MkAnnotation name' Nothing], HLIR.MkExprMatch (HLIR.MkExprVariable (HLIR.MkAnnotation name' Nothing)) [(p, acc, Nothing)])
      )
      (mempty, expr)
      args'

  ty <-
    List.foldlM
      ( \acc (opt, x) -> do
          let HLIR.MkAnnotation name' ty = x
          ty' <- maybe M.fresh pure ty
          pure $ HLIR.MkTyRowExtend name' (HLIR.MkTyApp (HLIR.MkTyId "Optional") [ty']) opt acc
      )
      HLIR.MkTyRowEmpty
      labels

  let funTy =
        (HLIR.:->:)
          <$> ( (++)
                  <$> mapM HLIR.value args''
                  <*> pure [HLIR.MkTyRecord ty]
              )
          <*> ret

  pure ((start, end), HLIR.MkExprLet (fromList generics) (Left (HLIR.MkAnnotation name funTy)) (HLIR.MkExprLambda (args'' ++ [HLIR.MkAnnotation "kwargs" (Just (HLIR.MkTyRecord ty))]) ret body') (HLIR.MkExprVariable (HLIR.MkAnnotation "unit" Nothing)))

-- | PARSE LAMBDA EXPRESSION
-- | Parse a lambda expression. A lambda expression is an expression that consists
-- | of a lambda function definition. It is used to define an anonymous function in
-- | Bonzai.
-- | The syntax of a lambda expression is as follows:
-- |
-- | "fn" "(" arguments ")" (":" ret)? "=>" expression
parseLambda :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseLambda = do
  ((start, _), _) <- Lex.reserved "fn"
  (_, args) <-
    Lex.parens
      ( P.sepBy
          argument
          Lex.comma
      )
  ret <- P.option Nothing $ Just . snd <$> (Lex.symbol ":" *> Typ.parseType)

  void $ Lex.symbol "=>"

  let args' = (Left <$> lefts' args) <> (Right <$> rights' args)
  let labels = middles' args

  ((_, end), body) <- parseExpression

  (args'', body') <-
    List.foldlM
      ( \(vars, acc) x -> case x of
          Right a -> pure (vars <> [a], acc)
          Left (HLIR.MkPatVariable name _) ->
            pure (vars <> [HLIR.MkAnnotation name Nothing], acc)
          Left (HLIR.MkPatLocated p _)
            | Just name <- getPatVar p ->
                pure (vars <> [HLIR.MkAnnotation name Nothing], acc)
          Left p -> do
            name <- freshSymbol
            pure (vars <> [HLIR.MkAnnotation name Nothing], HLIR.MkExprMatch (HLIR.MkExprVariable (HLIR.MkAnnotation name Nothing)) [(p, acc, Nothing)])
      )
      (mempty, body)
      args'

  ty <-
    List.foldlM
      ( \acc (opt, x) -> do
          let HLIR.MkAnnotation name' ty = x
          ty' <- maybe M.fresh pure ty
          pure $ HLIR.MkTyRowExtend name' (HLIR.MkTyApp (HLIR.MkTyId "Optional") [ty']) opt acc
      )
      HLIR.MkTyRowEmpty
      labels

  pure ((start, end), HLIR.MkExprLambda (args'' ++ [HLIR.MkAnnotation "kwargs" (Just (HLIR.MkTyRecord ty))]) ret body')

-- | PARSE UPDATE EXPRESSION
-- | Parse an update expression. An update expression is an expression that consists
-- | of an update statement. It is used to update a value in Bonzai.
-- | The syntax of an update expression is as follows:
-- |
-- | identifier "=" expression
parseUpdate :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseUpdate = do
  ((start, _), name) <- P.try $ (Lex.identifier <|> Lex.parens (snd <$> Lex.operator)) <* Lex.symbol "="

  ((_, end), expr) <- parseExpression

  pure ((start, end), HLIR.MkExprUpdate (HLIR.MkUpdtVariable (HLIR.MkAnnotation name Nothing)) expr)

-- | PARSE REQUIRE
-- | Parse a require expression. A require expression is an expression that consists
-- | of a require statement. It is used to import a module in Bonzai.
-- | The syntax of a require expression is as follows:
-- |
-- | "require" string (":" identifier ("," identifier)*)?
parseRequire :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseRequire = do
  ((start, _), _) <- Lex.reserved "require"

  ((_, end'), path) <- Lex.lexeme Lit.parseString

  (pos, vars) <- P.option ((start, end'), []) $ do
    void $ Lex.symbol ":"
    vars <- P.sepBy1 Lex.identifier Lex.comma

    case viaNonEmpty last vars of
      Just ((_, end), _) -> do
        pure ((start, end), map snd vars)
      Nothing -> pure ((start, end'), [])

  pure (pos, HLIR.MkExprRequire path (fromList vars))

-- | PARSE SPAWN EXPRESSION
-- | Parse a spawn expression. A spawn expression is an expression that consists of
-- | a spawn statement. It is used to spawn a new thread in Bonzai.
-- | The syntax of a spawn expression is as follows:
-- |
-- | "spawn" expression
parseSpawn :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseSpawn = do
  ((start, _), _) <- Lex.reserved "spawn"
  ((_, end), expr) <- parseExpression

  pure
    ( (start, end),
      HLIR.MkExprApplication
        (HLIR.MkExprVariable (HLIR.MkAnnotation "Thread::new" Nothing))
        [ HLIR.MkExprLambda [] Nothing expr,
          HLIR.MkExprRecordEmpty
        ]
    )

-- | PARSE TERM EXPRESSION
-- | Parse a term expression. A term expression is an expression that consists of a term.
-- | It is used to represent a non-recursive value in Bonzai.
parseTerm :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseTerm =
  Lex.locateWith
    <$> P.choice
      [ parseLambda,
        parseLet,
        parseSpawn,
        P.try parseMut,
        parseMutExpr,
        parseMatch,
        parseTernary,
        parseLiteral,
        P.try parseExprRecord,
        parseBlock,
        parseList,
        P.try parseTuple,
        parseVariable,
        snd <$> Lex.parens parseExpression
      ]

parseStatement :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseStatement =
  Lex.locateWith
    <$> P.choice
      [ parseWhile,
        parseForIn,
        parseFunction,
        P.try parseUpdate,
        parseExpression
      ]

some' :: HLIR.HLIR "expression" -> HLIR.HLIR "expression"
some' e = HLIR.MkExprApplication (HLIR.MkExprVariable (HLIR.MkAnnotation "Some" Nothing)) [e, HLIR.MkExprRecordEmpty]

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
makeOperator ::
  Text ->
  (HLIR.Position, HLIR.HLIR "expression") ->
  (HLIR.Position, HLIR.HLIR "expression") ->
  (HLIR.Position, HLIR.HLIR "expression")
makeOperator op ((start, _), a) ((_, end), b) =
  ( (start, end),
    HLIR.MkExprBinary
      op
      a
      b
  )

parseExpression :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseExpression = Lex.locateWith <$> P.makeExprParser parseTerm table
  where
    table =
      [ [ P.InfixN $ Lex.symbol "+=" >> pure (makeOperator "+="),
          P.InfixN $ Lex.symbol "-=" >> pure (makeOperator "-="),
          P.InfixN $ Lex.symbol "*=" >> pure (makeOperator "*="),
          P.InfixN $ Lex.symbol "/=" >> pure (makeOperator "/="),
          P.InfixN $ Lex.symbol "%=" >> pure (makeOperator "%="),
          P.InfixN $ Lex.symbol "&=" >> pure (makeOperator "&="),
          P.InfixN $ Lex.symbol "|=" >> pure (makeOperator "|="),
          P.InfixN $ Lex.symbol "^=" >> pure (makeOperator "^="),
          P.InfixN $ Lex.symbol "<<=" >> pure (makeOperator "<<="),
          P.InfixN $ Lex.symbol ">>=" >> pure (makeOperator ">>=")
        ],
        [ P.Postfix . Lex.makeUnaryOp $ do
            ((_, end), field) <- P.string "->" *> Lex.nonLexedID <* Lex.scn
            pure $ \((start, _), e) -> ((start, end), HLIR.MkExprRecordAccess e field)
        ],
        [ P.Postfix . Lex.makeUnaryOp $ do
            let labelledArgument = do
                  (_, name) <- Lex.identifier
                  void $ Lex.symbol ":"
                  (_, expr) <- parseExpression
                  pure $ Right (name, expr)

            ((_, end), args) <- Lex.parens (P.sepBy (P.try labelledArgument <|> Left . snd <$> parseExpression) Lex.comma)

            let labelledArgs = rights args
                positionalArgs = lefts args

            let label = List.foldl (\acc (name, expr) -> HLIR.MkExprRecordExtension acc name False (some' expr)) HLIR.MkExprRecordEmpty labelledArgs

            pure $ \((start, _), e) -> ((start, end), HLIR.MkExprApplication e (positionalArgs ++ [label]))
        ],
        [ P.Postfix . Lex.makeUnaryOp $ do
            let labelledArgument = do
                  (_, name) <- Lex.identifier
                  void $ Lex.symbol ":"
                  (_, expr) <- parseExpression
                  pure $ Right (name, expr)

            ((_, end'), field) <- P.char '.' *> Lex.nonLexedID <* Lex.scn
            (end, args) <- P.option (end', []) $ do
              ((_, end), args) <- Lex.parens (P.sepBy (P.try labelledArgument <|> Left . snd <$> parseExpression) Lex.comma)

              pure (end, args)

            let labelledArgs = rights args
                positionalArgs = lefts args

            let label = List.foldl (\acc (name, expr) -> HLIR.MkExprRecordExtension acc name False (some' expr)) HLIR.MkExprRecordEmpty labelledArgs

            let var = HLIR.MkExprVariable (HLIR.MkAnnotation field Nothing)

            pure $ \((start, _), e) -> ((start, end), HLIR.MkExprApplication var (e : positionalArgs ++ [label]))
        ],
        [ P.Postfix . Lex.makeUnaryOp $ do
            void $ Lex.symbol "["
            (_, idx) <- parseExpression
            ((_, end), _) <- Lex.symbol "]"

            pure $ \((start, _), e) -> ((start, end), HLIR.MkExprIndex e idx)
        ],
        [ P.InfixL $ do
            void $ Lex.symbol "*"
            pure $ makeOperator "*",
          P.InfixL $ do
            void $ Lex.symbol "/"
            pure $ makeOperator "/"
        ],
        [ P.InfixL $ do
            void $ Lex.symbol "+"
            pure $ makeOperator "+",
          P.InfixL $ do
            void $ Lex.symbol "-"
            pure $ makeOperator "-"
        ],
        [ P.InfixN $ do
            void $ Lex.symbol "=="
            pure $ makeOperator "==",
          P.InfixN $ do
            void $ Lex.symbol "!="
            pure $ makeOperator "!="
        ],
        [ P.InfixN $ do
            void $ Lex.symbol ">="
            pure $ makeOperator ">=",
          P.InfixN $ do
            void $ Lex.symbol "<="
            pure $ makeOperator "<=",
          P.InfixN $ do
            void $ Lex.symbol ">"
            pure $ makeOperator ">",
          P.InfixN $ do
            void $ Lex.symbol "<"
            pure $ makeOperator "<"
        ],
        [ P.InfixL $ do
            void $ Lex.symbol "&&"
            pure $ makeOperator "&&",
          P.InfixL $ do
            void $ Lex.symbol "||"
            pure $ makeOperator "||"
        ],
        [ P.Prefix . Lex.makeUnaryOp $ do
            ((start, _), _) <- Lex.symbol "!"
            pure $ \((_, end), a) -> ((start, end), HLIR.MkExprApplication (HLIR.MkExprVariable (HLIR.MkAnnotation "!" Nothing)) [a, HLIR.MkExprRecordEmpty])
        ],
        [ P.InfixL $ do
            (_, op) <- Lex.operator
            pure $ makeOperator op
        ],
        [ P.InfixL $ do
            Lex.scn
            void $ P.char ':'
            (_, name) <- Lex.nonLexedID
            void $ P.char ':'
            Lex.scn

            pure $ makeOperator name
        ]
      ]

-- | PARSE TOPLEVEL
-- | Parse a toplevel expression. A toplevel expression is an expression that is
-- | at the top level of a module. It is used to define a module in Bonzai.
parseToplevel :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseToplevel =
  Lex.locateWith
    <$> P.choice
      [ parsePublic,
        P.try parseDatatype,
        parseDirectData,
        parseRequire,
        parseExtern,
        parseStatement
      ]

-- | PARSE PROGRAM
-- | Parse a program. A program is a list of toplevel expressions. It is used to
-- | define a module in Bonzai.
parseProgram :: (MonadIO m) => P.Parser m [HLIR.HLIR "expression"]
parseProgram = Lex.scn *> P.many (snd <$> parseToplevel) <* P.eof
