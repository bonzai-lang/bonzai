{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char qualified as Char
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Row
import Data.Text qualified as Text
import GHC.IO (unsafePerformIO)
import Language.Bonzai.Frontend.Module.Conversion (ModuleState (MkModuleState), moduleState, removeRequires, resolve, resolveContent, resultState)
import Language.Bonzai.Frontend.Parser hiding (Label, parse)
import Language.Bonzai.Frontend.Parser.Lexer qualified as Lex
import Language.Bonzai.Frontend.Typechecking.Checker (decomposeHeader, runTypechecking)
import Language.Bonzai.Syntax.HLIR qualified as HLIR
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Directory.Internal.Prelude (getEnv)
import System.Exit (ExitCode (ExitFailure))
import System.FilePath (dropExtension, isExtensionOf, makeRelative, takeDirectory, takeFileName, (</>))

{-# NOINLINE lastContent #-}
lastContent :: IORef (Map Text Text)
lastContent = unsafePerformIO $ newIORef mempty

{-# NOINLINE lastValidAST #-}
lastValidAST :: IORef (Map Text [HLIR.TLIR "expression"])
lastValidAST = unsafePerformIO $ newIORef mempty

isUnit :: HLIR.TLIR "expression" -> Bool
isUnit (HLIR.MkExprVariable (HLIR.MkAnnotation "unit" _)) = True
isUnit (HLIR.MkExprLoc e _) = isUnit e
isUnit _ = False

getVar :: HLIR.TLIR "expression" -> (Position, Uri) -> Maybe (Text, HLIR.Type)
getVar (HLIR.MkExprLiteral _) _ = Nothing
getVar (HLIR.MkExprInterface _ _) _ = Nothing
getVar (HLIR.MkExprData _ _) _ = Nothing
getVar (HLIR.MkExprPublic _) _ = Nothing
getVar (HLIR.MkExprRecordExtension e _ _ v) pos = do
  let vars = getVar e pos
  let vars' = getVar v pos
  vars <|> vars'
getVar HLIR.MkExprRecordEmpty _ = Nothing
getVar (HLIR.MkExprRecordAccess e _) pos = do
  let vars = getVar e pos
  vars
getVar (HLIR.MkExprSingleIf c e) pos = do
  let vars = getVar c pos
  let vars' = getVar e pos
  vars <|> vars'
getVar (HLIR.MkExprLet _ (Left ann) e b) p | isUnit b = case getVar e p of
  Just (name, ty) -> Just (name, ty)
  Nothing -> Just (HLIR.name ann, runIdentity $ HLIR.value ann)
getVar (HLIR.MkExprLet _ (Left ann) e b) p = case getVar e p of
  Just (name, ty) -> Just (name, ty)
  Nothing -> case getVar b p of
    Just (name, ty) -> Just (name, ty)
    Nothing -> Just (HLIR.name ann, runIdentity $ HLIR.value ann)
getVar (HLIR.MkExprLet _ (Right pat) e b) p = case getVar e p of
  Just (name, ty) -> Just (name, ty)
  Nothing -> case getVar b p of
    Just (name, ty) -> Just (name, ty)
    Nothing -> getVarInPattern pat p
getVar (HLIR.MkExprNative ann ty) _ = Just (HLIR.name ann, ty)
getVar (HLIR.MkExprVariable ann) _ = Just (HLIR.name ann, runIdentity $ HLIR.value ann)
getVar (HLIR.MkExprLoc e (start, end)) ref = do
  let (Position line col, uri) = ref
  let pos' =
        SourcePos
          (fromMaybe "" (uriToFilePath uri))
          (mkPos $ fromIntegral line)
          (mkPos $ fromIntegral col)

  let cond = pos' >= start && pos' < end

  if cond then getVar e ref else Nothing
getVar (HLIR.MkExprLambda _ _ body) pos = getVar body pos
getVar (HLIR.MkExprMatch e cs) pos = do
  let (Position line col, uri) = pos
  let pos' =
        SourcePos
          (fromMaybe "" (uriToFilePath uri))
          (mkPos $ fromIntegral line)
          (mkPos $ fromIntegral col)

  let vars =
        map
          ( \(pat, e', p) -> do
              case p of
                Just (start, end) ->
                  if pos' >= start && pos' < end
                    then getVarInPattern pat pos
                    else getVar e' pos
                Nothing -> getVar e' pos
          )
          cs
  case getVar e pos of
    Just x -> Just x
    Nothing -> case catMaybes vars of
      (x : _) -> Just x
      [] -> Nothing
getVar (HLIR.MkExprTernary c t e) pos = getVar c pos <|> getVar t pos <|> getVar e pos
getVar (HLIR.MkExprBlock es) pos = foldr (\e acc -> getVar e pos <|> acc) Nothing es
getVar (HLIR.MkExprApplication f args) pos =
  case foldr (\e acc -> getVar e pos <|> acc) Nothing args of
    Just x -> Just x
    Nothing -> getVar f pos
getVar (HLIR.MkExprList l) pos = foldr (\e acc -> getVar e pos <|> acc) Nothing l
getVar (HLIR.MkExprWhile c b) pos = getVar b pos <|> getVar c pos
getVar (HLIR.MkExprUpdate up val) pos = do
  case getVar val pos of
    Just x -> Just x
    Nothing -> case up of
      HLIR.MkUpdtVariable name -> Just (HLIR.name name, runIdentity $ HLIR.value name)
      _ -> Nothing
getVar (HLIR.MkExprMut e) pos = getVar e pos
getVar (HLIR.MkExprIndex e i) pos = getVar i pos <|> getVar e pos
getVar (HLIR.MkExprRequire {}) _ = Nothing

getVarInPattern :: HLIR.TLIR "pattern" -> (Position, Uri) -> Maybe (Text, HLIR.Type)
getVarInPattern (HLIR.MkPatVariable name ty) _ = Just (name, runIdentity ty)
getVarInPattern (HLIR.MkPatLocated p (start, end)) ref = do
  let (Position line col, uri) = ref
  let pos' =
        SourcePos
          (fromMaybe "" (uriToFilePath uri))
          (mkPos $ fromIntegral line)
          (mkPos $ fromIntegral col)

  let cond = pos' >= start && pos' < end

  if cond
    then getVarInPattern p ref
    else Nothing
getVarInPattern (HLIR.MkPatList l sl _) pos = do
  case foldr (\p acc -> getVarInPattern p pos <|> acc) Nothing l of
    Just x -> Just x
    Nothing -> case sl of
      Just p -> getVarInPattern p pos
      Nothing -> Nothing
getVarInPattern (HLIR.MkPatConstructor _ args) pos =
  foldr (\p acc -> getVarInPattern p pos <|> acc) Nothing args
getVarInPattern _ _ = Nothing

getLetOrNat :: HLIR.TLIR "expression" -> Maybe (HLIR.TLIR "expression")
getLetOrNat l@(HLIR.MkExprLet {}) = Just l
getLetOrNat l@(HLIR.MkExprNative {}) = Just l
getLetOrNat (HLIR.MkExprLoc e _) = getLetOrNat e
getLetOrNat _ = Nothing

findLets ::
  HLIR.TLIR "expression" ->
  (Position, Uri) ->
  Maybe HLIR.Position ->
  Map Text (HLIR.Type, Maybe CompletionItemKind)
findLets (HLIR.MkExprVariable _) _ _ = mempty
findLets (HLIR.MkExprRequire _ _) _ _ = mempty
findLets (HLIR.MkExprLiteral _) _ _ = mempty
findLets (HLIR.MkExprInterface _ _) _ _ = mempty
findLets (HLIR.MkExprData _ _) _ _ = mempty
findLets (HLIR.MkExprRecordExtension e _ _ v) pos p = do
  let vars = findLets e pos p
  let vars' = findLets v pos p
  vars <> vars'
findLets HLIR.MkExprRecordEmpty _ _ = mempty
findLets (HLIR.MkExprRecordAccess e _) pos p = do
  let vars = findLets e pos p
  vars
findLets (HLIR.MkExprSingleIf c e) pos p = do
  let vars = findLets c pos p
  let vars' = findLets e pos p
  vars <> vars'
findLets (HLIR.MkExprPublic pb) (pos, uri) p@(Just (start, end))
  | Just e <- getLetOrNat pb = case e of
      HLIR.MkExprLet _ ann e' b -> do
        let Position line col = pos
        let pos' =
              SourcePos
                (fromMaybe "" (uriToFilePath uri))
                (mkPos $ fromIntegral line + 1)
                (mkPos $ fromIntegral col + 1)

        let cond =
              pos' >= start
                && pos' < end

        let varDef = case ann of
              Left ann' -> Map.singleton (HLIR.name ann') (runIdentity $ HLIR.value ann', Nothing)
              Right pat -> Map.map (,Nothing) (findLetsInPattern pat (pos, uri) p)

        if cond
          then findLets e' (pos, uri) p <> varDef <> findLets b (pos, uri) p
          else
            if sourceName start /= fromMaybe "" (uriToFilePath uri)
              then varDef
              else mempty
      (HLIR.MkExprNative ann ty) -> do
        Map.singleton (HLIR.name ann) (ty, Nothing)
      _ -> mempty
findLets (HLIR.MkExprPublic e) pos p = findLets e pos p
findLets (HLIR.MkExprLet _ ann e b) (pos, uri) p'@(Just (start, end)) = do
  let Position line col = pos
  let pos' =
        SourcePos
          (fromMaybe "" (uriToFilePath uri))
          (mkPos $ fromIntegral line + 1)
          (mkPos $ fromIntegral col + 1)

  let cond =
        pos' >= start
          && pos' < end

  let varDef = case ann of
        Left ann'' -> Map.singleton (HLIR.name ann'') (runIdentity $ HLIR.value ann'', Nothing)
        Right pat -> Map.map (,Nothing) (findLetsInPattern pat (pos, uri) p')

  if cond
    then findLets e (pos, uri) p' <> varDef <> findLets b (pos, uri) p'
    else
      if sourceName start /= fromMaybe "" (uriToFilePath uri)
        then varDef
        else mempty
findLets (HLIR.MkExprNative ann ty) (pos, uri) (Just (start, end)) = do
  let Position line col = pos
  let pos' =
        SourcePos
          (fromMaybe "" (uriToFilePath uri))
          (mkPos $ fromIntegral line + 1)
          (mkPos $ fromIntegral col + 1)

  let cond =
        pos' >= start
          && pos' < end

  if cond
    then Map.singleton (HLIR.name ann) (ty, Nothing)
    else mempty
findLets (HLIR.MkExprMut e) pos p = findLets e pos p
findLets (HLIR.MkExprLoc (HLIR.MkExprBlock es) p) pos _ =
  foldr (\e acc -> findLets e pos (Just p) <> acc) mempty es
findLets (HLIR.MkExprLoc e p'@(start, _)) pos p = case p of
  Just (_, end') -> findLets e pos (Just (start, end'))
  Nothing -> findLets e pos (Just p')
findLets (HLIR.MkExprLambda args _ body) p@(pos, uri) p'@(Just (start, end)) = do
  let Position line col = pos
  let pos' = SourcePos (fromMaybe "" (uriToFilePath uri)) (mkPos $ fromIntegral line + 1) (mkPos $ fromIntegral col + 1)

  let cond =
        pos' >= start
          && pos' < end
  let vars = Map.fromList $ map (\(HLIR.MkAnnotation name ty) -> (name, (runIdentity ty, Nothing))) args

  if cond
    then vars <> findLets body p p'
    else mempty
findLets (HLIR.MkExprLambda args _ body) p p' = do
  let vars = Map.fromList $ map (\(HLIR.MkAnnotation name ty) -> (name, (runIdentity ty, Nothing))) args

  vars <> findLets body p p'
findLets (HLIR.MkExprMatch e cs) pos p = do
  let vars = findLets e pos p
  let vars' = foldr (\(pat, expr, p') acc -> Map.map (,Nothing) (findLetsInPattern pat pos p') <> findLets expr pos p' <> acc) mempty cs
  vars <> vars'
findLets (HLIR.MkExprTernary c t e) pos p = do
  let vars = findLets c pos p
  let vars' = findLets t pos p
  let vars'' = findLets e pos p
  vars <> vars' <> vars''
findLets (HLIR.MkExprBlock es) pos p = do
  let vars = foldr (\e acc -> findLets e pos p <> acc) mempty es
  vars
findLets (HLIR.MkExprApplication f args) pos p = do
  let vars = findLets f pos p
  let vars' = foldr (\e acc -> findLets e pos p <> acc) mempty args
  vars <> vars'
findLets (HLIR.MkExprList l) pos p = do
  let vars = foldr (\e acc -> findLets e pos p <> acc) mempty l
  vars
findLets (HLIR.MkExprWhile c b) pos p = do
  let vars = findLets c pos p
  let vars' = findLets b pos p
  vars <> vars'
findLets (HLIR.MkExprUpdate up val) pos p = do
  let vars = findLets val pos p
  case up of
    HLIR.MkUpdtVariable name -> vars <> Map.singleton (HLIR.name name) (runIdentity $ HLIR.value name, Nothing)
    _ -> vars
findLets (HLIR.MkExprIndex e i) pos p = do
  let vars = findLets e pos p
  let vars' = findLets i pos p
  vars <> vars'
findLets _ _ _ = mempty

findLetsInPattern :: HLIR.TLIR "pattern" -> (Position, Uri) -> Maybe HLIR.Position -> Map Text HLIR.Type
findLetsInPattern (HLIR.MkPatVariable name ty) (pos, uri) (Just (start, end)) = do
  let Position line col = pos
  let pos' = SourcePos (fromMaybe "" (uriToFilePath uri)) (mkPos $ fromIntegral line + 1) (mkPos $ fromIntegral col + 1)

  let cond = pos' > start && pos' < end && sourceName start == fromMaybe "" (uriToFilePath uri)

  if cond
    then Map.singleton name (runIdentity ty)
    else mempty
findLetsInPattern (HLIR.MkPatLocated p (start, _)) ref (Just (_, end)) = do
  findLetsInPattern p ref (Just (start, end))
findLetsInPattern (HLIR.MkPatVariable name ty) _ _ = Map.singleton name (runIdentity ty)
findLetsInPattern (HLIR.MkPatLocated p _) ref p' = findLetsInPattern p ref p'
findLetsInPattern (HLIR.MkPatList l sl _) pos p' = do
  let vars = foldr (\p acc -> findLetsInPattern p pos p' <> acc) mempty l
  case sl of
    Just p -> vars <> findLetsInPattern p pos p'
    Nothing -> vars
findLetsInPattern (HLIR.MkPatConstructor _ args) pos p' =
  foldr (\p acc -> findLetsInPattern p pos p' <> acc) mempty args
findLetsInPattern _ _ _ = mempty

parseURI :: (MonadIO m) => Uri -> m (Maybe [HLIR.TLIR "expression"])
parseURI uri = do
  let path = fromJust $ uriToFilePath uri
  let folder = takeDirectory path
      fileNameWithoutDir = dropExtension $ takeFileName path

  writeIORef moduleState $
    MkModuleState
      fileNameWithoutDir
      folder
      mempty
      mempty
      mempty
  moduleResult <- runExceptT $ resolve fileNameWithoutDir True

  case moduleResult of
    Left _ -> do
      asts <- readIORef lastValidAST
      pure $ Map.lookup (toText path) asts
    Right _ -> do
      preHLIR <- removeRequires <$> readIORef resultState

      typed <- runTypechecking preHLIR
      case typed of
        Left _ -> do
          asts <- readIORef lastValidAST
          pure $ Map.lookup (toText path) asts
        Right tlir -> pure $ Just tlir

getFiles :: (MonadIO m) => FilePath -> m [FilePath]
getFiles path = do
  isFile <- liftIO $ doesFileExist path
  if isFile
    then pure [path]
    else do
      isDir <- liftIO $ doesDirectoryExist path
      if isDir
        then do
          contents <- liftIO $ listDirectory path
          let paths =
                filter (".bzi" `isExtensionOf`) $
                  map (path </>) contents
          folders <-
            filterM
              (\p -> liftIO $ doesDirectoryExist (path </> p))
              contents
          files <- mapM (getFiles . (path </>)) folders
          pure $ concat files <> paths
        else pure []

getStdFiles :: (MonadIO m) => FilePath -> m [FilePath]
getStdFiles stdPath = do
  let path = stdPath </> "standard"
  getFiles path

findInterface :: HLIR.TLIR "expression" -> Text -> Maybe [HLIR.Annotation HLIR.Type]
findInterface (HLIR.MkExprInterface ann events) name | HLIR.name ann == name = Just events
findInterface (HLIR.MkExprLoc e _) name = findInterface e name
findInterface _ _ = Nothing

parseContentAndTypecheck :: (MonadIO m) => FilePath -> Text -> m (Either (Text, HLIR.Position) [HLIR.TLIR "expression"])
parseContentAndTypecheck path contents = do
  let folder = takeDirectory path
      fileNameWithoutDir = dropExtension $ takeFileName path

  writeIORef resultState []
  writeIORef moduleState $
    MkModuleState
      fileNameWithoutDir
      folder
      mempty
      mempty
      mempty
  moduleResult <- runExceptT $ resolveContent contents

  case moduleResult of
    Left (err, pos) -> pure $ Left (show err, pos)
    Right _ -> do
      preHLIR <- removeRequires <$> readIORef resultState

      typed <- runTypechecking preHLIR
      case typed of
        Left (err, pos) -> pure $ Left (show err, pos)
        Right tlir -> do
          modifyIORef lastValidAST (Map.insert (toText path) tlir)
          pure $ Right tlir

parseAndTypecheck :: (MonadIO m) => FilePath -> m (Either (Text, HLIR.Position) [HLIR.TLIR "expression"])
parseAndTypecheck path = do
  let folder = takeDirectory path
      fileNameWithoutDir = dropExtension $ takeFileName path

  writeIORef resultState []
  writeIORef moduleState $
    MkModuleState
      fileNameWithoutDir
      folder
      mempty
      mempty
      mempty
  moduleResult <- runExceptT $ resolve fileNameWithoutDir True

  case moduleResult of
    Left (err, pos) -> pure $ Left (show err, pos)
    Right _ -> do
      preHLIR <- removeRequires <$> readIORef resultState

      typed <- runTypechecking preHLIR
      case typed of
        Left (err, pos) -> pure $ Left (show err, pos)
        Right tlir -> do
          modifyIORef lastValidAST (Map.insert (toText path) tlir)

          pure $ Right tlir

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_not -> pure (),
      notificationHandler SMethod_SetTrace $ \_not -> pure (),
      notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_not -> pure (),
      notificationHandler SMethod_TextDocumentDidClose $ \_req -> do
        let TNotificationMessage _ _ (DidCloseTextDocumentParams (TextDocumentIdentifier uri)) = _req

        let path = toText . fromJust $ uriToFilePath uri

        modifyIORef lastContent (Map.delete path)
        modifyIORef lastValidAST (Map.delete path),
      notificationHandler SMethod_TextDocumentDidOpen $ \req -> do
        let TNotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _ _ _)) = req

        let path = fromJust $ uriToFilePath uri

        -- check if path ends with .bzi

        when (isExtensionOf ".bzi" path) $ do
            content <- decodeUtf8 <$> readFileLBS path

            modifyIORef lastContent (Map.insert (toText path) content)

            ast <- parseAndTypecheck path

            case ast of
              Left (err, (p1, p2)) -> do
                let (l1, c1) = (unPos (sourceLine p1), unPos (sourceColumn p1))
                let (l2, c2) = (unPos (sourceLine p2), unPos (sourceColumn p2))

                sendNotification SMethod_TextDocumentPublishDiagnostics $
                  PublishDiagnosticsParams
                    uri
                    Nothing
                    [ Diagnostic
                        { _range = Range (Position (fromIntegral l1 - 1) (fromIntegral c1 - 1)) (Position (fromIntegral l2 - 1) (fromIntegral c2 - 1)),
                          _severity = Just DiagnosticSeverity_Error,
                          _code = Nothing,
                          _source = Just "Bonzai",
                          _message = err,
                          _relatedInformation = Nothing,
                          _tags = Nothing,
                          _data_ = Nothing,
                          _codeDescription = Nothing
                        }
                    ]
              Right _ -> do
                sendNotification SMethod_TextDocumentPublishDiagnostics $ PublishDiagnosticsParams uri Nothing [],
      notificationHandler
        SMethod_TextDocumentDidChange
        $ \req -> do
          let TNotificationMessage _ _ (DidChangeTextDocumentParams (VersionedTextDocumentIdentifier uri _) updates) = req

          -- Get updated text content

          let content =
                foldr
                  ( \(TextDocumentContentChangeEvent opt) acc -> case opt of
                      InR ((Label :: Label "text") :== text) -> acc <> text
                      InL
                        ( ((Label :: Label "range") :== _)
                            :+ ((Label :: Label "rangeLength") :== _)
                            :+ (Label :== text)
                          ) -> acc <> text
                  )
                  ""
                  updates
          let path = fromJust $ uriToFilePath uri

          when (isExtensionOf "bzi" path) $ do
              modifyIORef lastContent (Map.insert (toText path) content)

              ast <- parseContentAndTypecheck path content

              case ast of
                Left (err, (p1, p2)) -> do
                  let (l1, c1) = (unPos (sourceLine p1), unPos (sourceColumn p1))
                  let (l2, c2) = (unPos (sourceLine p2), unPos (sourceColumn p2))

                  sendNotification SMethod_TextDocumentPublishDiagnostics $
                    PublishDiagnosticsParams
                      uri
                      Nothing
                      [ Diagnostic
                          { _range = Range (Position (fromIntegral l1 - 1) (fromIntegral c1 - 1)) (Position (fromIntegral l2 - 1) (fromIntegral c2 - 1)),
                            _severity = Just DiagnosticSeverity_Error,
                            _code = Nothing,
                            _source = Just "Bonzai",
                            _message = err,
                            _relatedInformation = Nothing,
                            _tags = Nothing,
                            _data_ = Nothing,
                            _codeDescription = Nothing
                          }
                      ]
                Right _ -> do
                  sendNotification SMethod_TextDocumentPublishDiagnostics $ PublishDiagnosticsParams uri Nothing [],
      requestHandler SMethod_TextDocumentHover $ \req responder -> do
        let TRequestMessage
              _
              _
              _
              (HoverParams (TextDocumentIdentifier uri) pos _workDone) = req
        let Position line col = pos

        asts <- readIORef lastValidAST
        let tlir = Map.lookup (toText . fromJust $ uriToFilePath uri) asts

        case tlir of
          Nothing -> responder (Right $ InR Null)
          Just tlir' -> do
            let vars =
                  mapMaybe
                    (\e -> getVar e (Position (line + 1) (col + 1), uri))
                    tlir'

            case vars of
              [] -> responder (Right $ InR Null)
              (x : _) -> case x of
                (name, ty) -> do
                  ty' <- HLIR.simplify ty

                  let md =
                        mkMarkdown $
                          "```bonzai\n"
                            <> name
                            <> " : "
                            <> toText ty'
                            <> "\n```"

                  let range =
                        Range
                          (Position line $ fromIntegral col)
                          (Position line $ fromIntegral col)
                  let rsp = Hover (InL md) (Just range)
                  responder (Right $ InL rsp),
      requestHandler SMethod_TextDocumentCompletion $ \req responder -> do
        let TRequestMessage
              _
              _
              _
              (CompletionParams (TextDocumentIdentifier uri) pos@(Position line col) _ _ _) =
                req

        -- Get current text content with all unsaved modifications

        tlirs <- readIORef lastContent
        let content = fromMaybe "" $ Map.lookup (toText . fromJust $ uriToFilePath uri) tlirs

        asts <- readIORef lastValidAST
        let ast' = fromMaybe [] $ Map.lookup (toText . fromJust $ uriToFilePath uri) asts

        let completion label' kind detail txt =
              CompletionItem
                { _label = label',
                  _kind = Just kind,
                  _detail = Just detail,
                  _data_ = Nothing,
                  _documentation = Nothing,
                  _sortText = Nothing,
                  _filterText = Nothing,
                  _insertText = txt,
                  _insertTextFormat = Nothing,
                  _textEdit = Nothing,
                  _additionalTextEdits = Nothing,
                  _commitCharacters = Nothing,
                  _command = Nothing,
                  _tags = Nothing,
                  _deprecated = Nothing,
                  _preselect = Nothing,
                  _labelDetails = Nothing,
                  _insertTextMode = Nothing,
                  _textEditText = Nothing
                }

        let lines' = lines content

        let currentLine = fromIntegral line `maybeAt` lines'

        case currentLine of
          Just currentLine' -> do
            let before = Text.take (fromIntegral col) currentLine'
            let isArrow = (Text.length before >= 2) && (Text.takeEnd 2 before == "->")

            let before' = Text.dropWhileEnd Char.isSpace before
            let isRequire = Text.length before' >= 7 && Text.take 7 before' == "require"

            if
              | isArrow -> do
                  let var = Text.takeWhileEnd Lex.isIdentChar (Text.dropEnd 2 before)

                  let colStart = col - fromIntegral (Text.length var) - 2
                  let vars = mapMaybe (`getVar` (Position line (colStart + 1), uri)) ast'

                  case vars of
                    ((_, ty) : _) -> do
                      res <- runExceptT $ decomposeHeader ty
                      let (header, _) = fromRight ("", mempty) res
                      let interfaces = mapMaybe (`findInterface` header) ast'

                      case interfaces of
                        (events : _) -> do
                          let completions = map (\(HLIR.MkAnnotation name ty') -> completion name CompletionItemKind_Event (toText ty') Nothing) events
                          responder $ Right $ InL completions
                        [] -> responder $ Right $ InL []
                    [] -> responder $ Right $ InL []
              | isRequire -> do
                  let filePath = fromMaybe "" $ uriToFilePath uri
                  let folder = takeDirectory filePath

                  files <- getFiles folder

                  stdPath <- liftIO $ getEnv "BONZAI_PATH"

                  stdFiles <- getStdFiles stdPath

                  let stdRelFiles = map (("std:" <>) . makeRelative (stdPath </> "standard")) stdFiles
                  let relFiles = map (makeRelative folder) files

                  let completions = zipWith (\rf af -> completion (toText rf) CompletionItemKind_File (toText af) (Just . show . toText $ dropExtension rf)) relFiles files

                  let stdCompletions = zipWith (\rf af -> completion (toText rf) CompletionItemKind_Module (toText af) (Just . show . toText $ dropExtension rf)) stdRelFiles stdFiles

                  responder $ Right $ InL (completions <> stdCompletions)
              | otherwise -> do
                  let vars = Map.unions $ map (\e -> findLets e (pos, uri) Nothing) ast'
                  vars' <- mapM (\(t, c) -> (,c) <$> HLIR.simplify t) vars

                  let completions =
                        Map.foldrWithKey
                          ( \k (v, c) acc ->
                              completion
                                k
                                ( case c of
                                    Just c' -> c'
                                    Nothing -> case v of
                                      _ HLIR.:->: _ -> CompletionItemKind_Function
                                      _ -> CompletionItemKind_Variable
                                )
                                (toText v)
                                Nothing
                                : acc
                          )
                          []
                          vars'

                  responder $ Right $ InL completions
          Nothing -> do
            let vars = Map.unions $ map (\e -> findLets e (pos, uri) Nothing) ast'
            vars' <- mapM (\(t, c) -> (,c) <$> HLIR.simplify t) vars

            let completions =
                  Map.foldrWithKey
                    ( \k (v, c) acc ->
                        completion
                          k
                          ( case c of
                              Just c' -> c'
                              Nothing -> case v of
                                _ HLIR.:->: _ -> CompletionItemKind_Function
                                _ -> CompletionItemKind_Variable
                          )
                          (toText v)
                          Nothing
                          : acc
                    )
                    []
                    vars'

            responder $ Right $ InL completions,
      notificationHandler SMethod_TextDocumentDidSave $ \req -> do
        let TNotificationMessage _ _ (DidSaveTextDocumentParams (TextDocumentIdentifier uri) _) = req

        let path = fromJust $ uriToFilePath uri

        when (isExtensionOf "bzi" path) $ do
            ast <- parseAndTypecheck path

            case ast of
              Left (err, (p1, p2)) -> do
                let (l1, c1) = (unPos (sourceLine p1), unPos (sourceColumn p1))
                let (l2, c2) = (unPos (sourceLine p2), unPos (sourceColumn p2))

                sendNotification SMethod_TextDocumentPublishDiagnostics $
                  PublishDiagnosticsParams
                    uri
                    Nothing
                    [ Diagnostic
                        { _range = Range (Position (fromIntegral l1 - 1) (fromIntegral c1 - 1)) (Position (fromIntegral l2 - 1) (fromIntegral c2 - 1)),
                          _severity = Just DiagnosticSeverity_Error,
                          _code = Nothing,
                          _source = Just "Bonzai",
                          _message = err,
                          _relatedInformation = Nothing,
                          _tags = Nothing,
                          _data_ = Nothing,
                          _codeDescription = Nothing
                        }
                    ]
              Right _ -> do
                sendNotification SMethod_TextDocumentPublishDiagnostics $ PublishDiagnosticsParams uri Nothing []
    ]

syncOptions :: TextDocumentSyncOptions
syncOptions =
  TextDocumentSyncOptions
    { _openClose = Just True,
      _change = Just TextDocumentSyncKind_Full,
      _willSave = Just False,
      _willSaveWaitUntil = Just False,
      _save = Just $ InR $ SaveOptions $ Just True
    }

lspOptions :: Options
lspOptions =
  defaultOptions
    { optTextDocumentSync = Just syncOptions,
      optExecuteCommandCommands = Nothing
    }

main :: IO Int
main = do
  exitCode <-
    runServer $
      ServerDefinition
        { parseConfig = const $ const $ Right (),
          onConfigChange = const $ pure (),
          defaultConfig = (),
          configSection = "demo",
          doInitialize = \env _req -> pure $ Right env,
          staticHandlers = const handlers,
          interpretHandler = \env -> Iso (runLspT env) liftIO,
          options = lspOptions
        }
  exitWith $ ExitFailure exitCode
