{-# LANGUAGE LambdaCase #-}
module Language.Bonzai.Backend.LLIR.Conversion where

-- | LLIR ASSEMBLER
-- | The LLIR assembler is a step in the compilation process where the AST is
-- | transformed into a simpler form, the LLIR (Low-Level Intermediate
-- | Representation). It is a mix of high-level constructs (variables,
-- | functions...) and low-level constructs (instructions, jumps...).
-- |
-- | Transforming our code to LLIR is quite straightforward, as we only need to
-- | transform our AST into a sequence of instructions.
-- | But they are rules to respect:
-- |
-- |  - We need to keep track of the constants we use in our code, in order to
-- |    have a constant table to serialize
-- |
-- |  - We need to keep track of the global variables we use in our code, in
-- |    order to know if we load a local, global, or native variable
-- |
-- |  - We need to keep track of the native functions we use in our code, in
-- |    order to know if we call a local, global, or native function. We need it
-- |    too to know the address of the native function in the final binary and
-- |    the address too of the library.
-- |
-- |  - We need to know which functions are returning something, to know if we 
-- |    need to put a relative jump or not.

import qualified Language.Bonzai.Syntax.MLIR as MLIR
import qualified Language.Bonzai.Syntax.LLIR as LLIR
import qualified GHC.IO as IO
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Result (compilerError)
import qualified Language.Bonzai.Backend.LLIR.Free as M
import qualified Data.List as List
import qualified Data.IntMap as IntMap
import qualified Language.Bonzai.Frontend.Parser as MP

{-# NOINLINE constantPool #-}
constantPool :: IORef (Map MLIR.Literal Int)
constantPool = IO.unsafePerformIO $ newIORef Map.empty

{-# NOINLINE globals #-}
globals :: IORef (Set Text)
globals = IO.unsafePerformIO $ newIORef Set.empty

{-# NOINLINE natives #-}
natives :: IORef (Set Text)
natives = IO.unsafePerformIO $ newIORef Set.empty

{-# NOINLINE eventPool #-}
eventPool :: IORef (Map Text Int)
eventPool = IO.unsafePerformIO $ newIORef mempty

{-# NOINLINE includeLocations #-}
includeLocations :: IORef Bool
includeLocations = IO.unsafePerformIO $ newIORef False

fetchEvent :: MonadIO m => Text -> m Int
fetchEvent name = do
  events <- readIORef eventPool
  case Map.lookup name events of
    Just i -> pure i
    Nothing -> do
      modifyIORef' eventPool (Map.insert name (Map.size events))
      pure (Map.size events)

{-# NOINLINE isFunctionCurrently #-}
isFunctionCurrently :: IORef Bool
isFunctionCurrently = IO.unsafePerformIO $ newIORef False

{-# NOINLINE nativeFunctionsHandler #-}
nativeFunctionsHandler :: IORef LLIR.Library
nativeFunctionsHandler = IO.unsafePerformIO $ newIORef $ LLIR.MkLibrary 0 mempty

fetchConstant :: MonadIO m => MLIR.Literal -> m Int
fetchConstant lit = do
  cnst <- readIORef constantPool
  case Map.lookup lit cnst of
    Just i -> pure i
    Nothing -> do
      modifyIORef' constantPool (Map.insert lit (Map.size cnst))
      pure (Map.size cnst)

class Assemble a where
  assemble :: MonadIO m => a -> ReaderT (Set Text) m [LLIR.Segment]

instance Assemble a => Assemble [a] where
  assemble = fmap concat . mapM assemble

shouldNotBeLabel :: LLIR.Segment -> Bool
shouldNotBeLabel (LLIR.Function {}) = False
shouldNotBeLabel _ = True

extractFrom :: LLIR.Segment -> [LLIR.Instruction]
extractFrom (LLIR.Function {}) = error "Not implemented"
extractFrom (LLIR.Event {}) = error "Not implemented"
extractFrom (LLIR.Instruction instr) = [instr]
extractFrom (LLIR.EventOn _ _ _ _ instrs) = instrs

instance Assemble MLIR.Expression where

  assemble (MLIR.MkExprUpdate (MLIR.MkUpdtVariable name) (MLIR.MkExprLambda args body)) = do
    body' <- assemble body

    let args' = Set.fromList args
    glbs <- readIORef globals
    ntvs <- readIORef natives
    let reserved = glbs <> ntvs

    let freed = M.free reserved body

    let env = freed <> args'

    let localSpaceSize = Set.size env

    let body'' = filter shouldNotBeLabel body'
    let finalBody = concatMap extractFrom body''

    let locals = List.nub $ args <> Set.toList freed
        localSpace = zip locals [0..]

    pure [LLIR.Function name args localSpaceSize localSpace finalBody]

  assemble (MLIR.MkExprFunction name args body) = do
    writeIORef isFunctionCurrently True
    glbs <- readIORef globals
    ntvs <- readIORef natives
    let reserved = glbs <> ntvs

    let args' = Set.fromList args
        freed = M.free reserved body
        env   = freed <> args'

    body' <- local (<> env) $ assemble body

    let localSpaceSize = Set.size env

    let body'' = filter shouldNotBeLabel body'
        finalBody = concatMap extractFrom body''

    let locals = List.nub $ args <> Set.toList freed
        localSpace = zip locals [0..]

    writeIORef isFunctionCurrently False

    pure [LLIR.Function name args localSpaceSize localSpace finalBody]

  assemble (MLIR.MkExprEventDef name body) = do
    writeIORef isFunctionCurrently True
    glbs <- readIORef globals
    ntvs <- readIORef natives
    let reserved = glbs <> ntvs

    let env = M.free reserved body

    let ons = getOns body

    ons' <- local (<> env) $ assemble ons

    let localSpaceSize = Set.size env

    let body'' = filter shouldNotBeLabel ons'

    let locals = List.nub $ Set.toList env
        localSpace = zip locals [0..]

    writeIORef isFunctionCurrently False

    pure [LLIR.Event name localSpaceSize localSpace body'' (length ons)]

  assemble (MLIR.MkExprLet n e) = do
    if isFunction e || isEvent e then do
      assemble (MLIR.MkExprLet n (removeLoc e))
    else do
      e' <- assemble e
      gs <- readIORef globals
      locals <- ask

      if Set.member n locals then
        pure $ e' <> LLIR.storeLocal n
      else if Set.member n gs then
        pure $ e' <> LLIR.storeGlobal n
      else compilerError $ "Variable " <> n <> " not found in globals, locals or natives"

    where
      isFunction (MLIR.MkExprLoc _ e') = isFunction e'
      isFunction (MLIR.MkExprLambda {}) = True
      isFunction _ = False

      isEvent (MLIR.MkExprEvent _) = True
      isEvent (MLIR.MkExprLoc _ e') = isEvent e'
      isEvent _ = False

      removeLoc (MLIR.MkExprLoc _ e') = removeLoc e'
      removeLoc e' = e'

  assemble (MLIR.MkExprVariable n) = do
    gs <- readIORef globals
    nats <- readIORef natives
    locals <- ask

    if Set.member n locals then
      pure $ LLIR.loadLocal n
    else if Set.member n gs then
      pure $ LLIR.loadGlobal n
    else if Set.member n nats then do
      i <- fetchConstant (MLIR.MkLitString n)
      pure $ LLIR.instr (LLIR.LoadNative i)
    else compilerError $ "Variable " <> n <> " not found in globals, locals or natives"

  assemble (MLIR.MkExprApplication f args) = do
    f' <- assemble f
    args' <- assemble args
    pure $ args' <> f' <> LLIR.instr (LLIR.Call (length args))

  assemble (MLIR.MkExprLambda _ _) = compilerError "Lambda in LLIR"

  assemble (MLIR.MkExprTernary c t e) = do
    c' <- assemble c
    t' <- assemble t
    e' <- assemble e

    pure $ c' <> LLIR.instr (LLIR.JumpIfFalse (length t' + 2)) <> t' <> LLIR.instr (LLIR.JumpRel (length e' + 1)) <> e'

  assemble (MLIR.MkExprUpdate u e) = do
    u' <- assemble u
    e' <- assemble e
    pure $ e' <> u' <> LLIR.instr LLIR.Update

  assemble (MLIR.MkExprMut e) = do
    e' <- assemble e

    pure $ e' <> LLIR.instr LLIR.MakeMutable

  assemble (MLIR.MkExprBlock es) = assemble es

  assemble (MLIR.MkExprEvent es) = do
    glbs <- readIORef globals
    ntvs <- readIORef natives
    let reserved = glbs <> ntvs
    let env = M.free reserved es

    let locals = Set.toList env
        localSpace = zip locals [0..]

    let ons = filter (\case MLIR.MkExprOn {} -> True; _ -> False) es
    let lets = filter (\case MLIR.MkExprLet {} -> True; _ -> False) es

    ons' <- local (<> env) $ assemble ons
    lets' <- local (<> env) $ assemble lets

    let len = length ons' + length lets'

    pure $ LLIR.instr (LLIR.MakeEvent len (length ons) localSpace) <> ons' <> lets'

  assemble (MLIR.MkExprOn event args body) = do
    glbs <- readIORef globals
    ntvs <- readIORef natives
    let reserved = glbs <> ntvs <> Set.fromList args
    let env = M.free reserved body

    let locals = args <> Set.toList env
        localSpace = zip locals [0..]

    body' <- assemble body
    event' <- fetchEvent event

    let instrs = filter shouldNotBeLabel body'
        instrs' = concatMap extractFrom instrs

    pure [LLIR.EventOn event' (length args) (length instrs' + 1) localSpace (instrs' <> [LLIR.Return])]

  assemble (MLIR.MkExprSend e event es) = do
    e' <- assemble e
    es' <- assemble es
    event' <- fetchEvent event

    pure $ e' <> es' <> LLIR.instr (LLIR.Send (length es) event')

  assemble (MLIR.MkExprSpawn e) = do
    e' <- assemble e
    pure $ e' <> LLIR.instr LLIR.Spawn

  assemble (MLIR.MkExprList es) = do
    es' <- assemble es
    pure $ es' <> LLIR.instr (LLIR.MakeList (length es))

  assemble (MLIR.MkExprNative ann ty) = do
    let arity = case ty of
          args MLIR.:->: _ -> length args
          _ -> 0

    modifyIORef' natives (Set.insert ann.name)
    modifyIORef' nativeFunctionsHandler $
      \(LLIR.MkLibrary i fs) ->
        LLIR.MkLibrary
          (i + 1)
          (Set.insert (LLIR.MkFunctionLibrary ann.name arity (i + 1)) fs)

    pure []

  assemble (MLIR.MkExprIndex e i) | isIntLiteral i = do
    e' <- assemble e
    let i' = getInteger i
    pure $ e' <> LLIR.instr (LLIR.ListGet (fromInteger i'))

  assemble (MLIR.MkExprIndex e i) = do
    e' <- assemble e
    i' <- assemble i
    pure $ e' <> i' <> LLIR.instr LLIR.GetIndex

  assemble (MLIR.MkExprUnpack n e e') = do
    e'' <- assemble e
    e''' <- assemble e'
    pure $ e'' <> LLIR.instr (LLIR.StoreLocal n) <> e'''

  assemble (MLIR.MkExprLiteral l) = do
    i <- fetchConstant l
    pure $ LLIR.instr (LLIR.LoadConstant i)

  assemble (MLIR.MkExprLoc _ (MLIR.MkExprLoc p e)) = assemble (MLIR.MkExprLoc p e)

  assemble (MLIR.MkExprLoc (p1, _) e) = do
    e' <- assemble e
    
    include <- readIORef includeLocations

    if include then do
      file <- fetchConstant (MLIR.MkLitString . toText $ p1.sourceName)
      pure $ LLIR.instr (LLIR.Loc (MP.unPos p1.sourceLine) (MP.unPos p1.sourceColumn) file) <> e'
    else pure e'

  assemble (MLIR.MkExprWhile c e) = do
    c' <- assemble c
    e' <- assemble e
    pure $ c' <> LLIR.instr (LLIR.JumpIfFalse (length e' + 2)) <> e' <> LLIR.instr (LLIR.JumpRel (-(length c' + length e' + 1)))

  assemble MLIR.MkExprSpecial = do
    pure (LLIR.instr LLIR.Special)

instance Assemble MLIR.Update where
  assemble (MLIR.MkUpdtVariable n) = do
    gs <- readIORef globals
    nats <- readIORef natives
    locals <- ask

    if Set.member n locals then
      pure $ LLIR.loadLocal n
    else if Set.member n gs then
      pure $ LLIR.loadGlobal n
    else if Set.member n nats then do
      i <- fetchConstant (MLIR.MkLitString n)
      pure $ LLIR.instr (LLIR.LoadNative i)
    else compilerError $ "Variable " <> n <> " not found in globals, locals or natives"

  assemble (MLIR.MkUpdtField {}) = compilerError "Field in LLIR"

  assemble (MLIR.MkUpdtIndex u e) = do
    u' <- assemble u
    e' <- assemble e
    pure $ u' <> e' <> LLIR.instr LLIR.GetIndex

runLLIRConversion :: MonadIO m => [MLIR.Expression] -> m ([LLIR.Segment], [MLIR.Literal], Set Text)
runLLIRConversion exprs = do
  let gs = getGlobals exprs
  writeIORef globals gs

  let ns = getNatives exprs
  writeIORef natives ns

  res <- runReaderT (assemble exprs) Set.empty

  consts <- readIORef constantPool
  let cs = IntMap.fromList $ invertList (Map.toList consts)

  pure (res, IntMap.elems cs, gs)

invertList :: [(b, a)] -> [(a, b)]
invertList = map (\(a, b) -> (b, a))

isIntLiteral :: MLIR.Expression -> Bool
isIntLiteral (MLIR.MkExprLiteral (MLIR.MkLitInt _)) = True
isIntLiteral (MLIR.MkExprLoc _ e) = isIntLiteral e
isIntLiteral _ = False

getInteger :: MLIR.Expression -> Integer
getInteger (MLIR.MkExprLiteral (MLIR.MkLitInt i)) = i
getInteger (MLIR.MkExprLoc _ e) = getInteger e
getInteger _ = error "Not an integer"

getOns :: [MLIR.Expression] -> [MLIR.Expression]
getOns (MLIR.MkExprOn n args e : xs) = MLIR.MkExprOn n args e : getOns xs
getOns (MLIR.MkExprLoc _ e : xs) = getOns (e : xs)
getOns (_ : xs) = getOns xs
getOns [] = []

getGlobals :: [MLIR.Expression] -> Set Text
getGlobals (MLIR.MkExprFunction name _ _ : es) = Set.insert name (getGlobals es)
getGlobals (MLIR.MkExprLet name e : xs) = Set.insert name (getGlobals (e:xs))
getGlobals (MLIR.MkExprMut e : xs) = getGlobals (e:xs)
getGlobals (MLIR.MkExprLoc _ e : xs) = getGlobals (e : xs)
getGlobals (MLIR.MkExprTernary c t e : xs) = getGlobals (c : t : e : xs)
getGlobals (MLIR.MkExprBlock es : xs) = getGlobals (es <> xs)
getGlobals (MLIR.MkExprApplication f args : xs) = getGlobals (f : args ++ xs)
getGlobals (MLIR.MkExprSend e _ es : xs) = getGlobals (e : es ++ xs)
getGlobals (MLIR.MkExprSpawn e : xs) = getGlobals (e : xs)
getGlobals (MLIR.MkExprList es : xs) = getGlobals (es ++ xs)
getGlobals (MLIR.MkExprIndex e i : xs) = getGlobals (e : i : xs)
getGlobals (MLIR.MkExprUnpack _ e e' : xs) = getGlobals (e : e' : xs)
getGlobals (MLIR.MkExprWhile c e : xs) = getGlobals (c : e : xs)
getGlobals (_ : xs) = getGlobals xs
getGlobals [] = mempty

getNatives :: [MLIR.Expression] -> Set Text
getNatives (MLIR.MkExprNative ann _ : xs) = Set.insert ann.name (getNatives xs)
getNatives (MLIR.MkExprLoc _ e : xs) = getNatives (e : xs)
getNatives (_ : xs) = getNatives xs
getNatives [] = mempty