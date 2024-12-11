{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.Bonzai.Backend.CLang.Generation where

import qualified Language.Bonzai.Syntax.CLang as CLang
import qualified Language.Bonzai.Syntax.TMLIR as TMLIR
import Control.Monad.Result (compilerError)
import qualified Language.Bonzai.Backend.Closure.Typed as TMLIR
import qualified GHC.IO as IO
import Data.List (partition)
import qualified Data.Text as Text
import qualified Data.Set as Set

{-# NOINLINE functions #-}
functions :: IORef [CLang.Toplevel]
functions = IO.unsafePerformIO $ newIORef mempty

{-# NOINLINE dataConstructors #-}
dataConstructors :: IORef (Set Text)
dataConstructors = IO.unsafePerformIO $ newIORef mempty

{-# NOINLINE typedefs #-}
typedefs :: IORef [CLang.Toplevel]
typedefs = IO.unsafePerformIO $ newIORef []

generateE :: MonadIO m => TMLIR.TMLIR "expression" -> m (CLang.Expression, [CLang.Toplevel])
generateE (TMLIR.MkExprLiteral l) = pure (CLang.MkExprLiteral l, [])
generateE (TMLIR.MkExprVariable v _) = do
  dcs <- readIORef dataConstructors

  if Set.member v dcs
    then pure (CLang.MkExprApplication (CLang.MkExprVariable v) [], [])
    else pure (CLang.MkExprVariable v, [])
generateE (TMLIR.MkExprLet {}) = compilerError "Let expressions are not supported in C"
generateE (TMLIR.MkExprLambda {}) = compilerError "Lambda expressions are not supported in C"
generateE (TMLIR.MkExprApplication (TMLIR.MkExprVariable "value" _) [e] _) = 
  generateE (TMLIR.MkExprUnref e)
generateE (TMLIR.MkExprApplication f args _) = do
  (f', hs) <- generateE f
  (args', hs') <- mapAndUnzipM generateE args
  pure (CLang.MkExprApplication f' args', hs <> concat hs')
generateE (TMLIR.MkExprBlock es t) = do
  (es', hs) <- mapAndUnzipM generateS es
  t' <- generateT Nothing t
  pure (CLang.MkExprBlock es' t', concat hs)
generateE (TMLIR.MkExprList es) = do
  (es', hs) <- mapAndUnzipM generateE es
  pure (CLang.MkExprList es', concat hs)
generateE (TMLIR.MkExprTernary c t e ty) = do
  (c', hs) <- generateE c
  (t', hs') <- generateE t
  (e', hs'') <- generateE e
  ty' <- generateT Nothing ty
  pure (CLang.MkExprTernary c' t' e' ty', hs <> hs' <> hs'')
generateE (TMLIR.MkExprUpdate u e) = do
  (u', hs) <- generateUpdate u
  (e', hs') <- generateE e
  pure (CLang.MkExprUpdate (CLang.MkUpdtUnref u') e', hs <> hs')
generateE (TMLIR.MkExprWhile {}) = compilerError "While expressions are not supported in C"
generateE (TMLIR.MkExprActor {}) = compilerError "Actor expressions are not supported in C"
generateE (TMLIR.MkExprSend {}) = compilerError "Send expressions are not supported in C"
generateE (TMLIR.MkExprSpawn _) = compilerError "Spawn expressions are not supported in C"
generateE (TMLIR.MkExprNative {}) = compilerError "Native expressions are not supported in C"
generateE (TMLIR.MkExprIndex e i) = do
  (e', hs) <- generateE e
  (i', hs') <- generateE i
  pure (CLang.MkExprIndex e' i', hs <> hs')
generateE (TMLIR.MkExprOn {}) = compilerError "On expressions are not supported in C"
generateE (TMLIR.MkExprField e f) = do
  (e', hs) <- generateE e
  pure (CLang.MkExprField e' f, hs)
generateE (TMLIR.MkExprUnpack {}) = compilerError "Unpack expressions are not supported in C"
generateE (TMLIR.MkExprInterface {}) = compilerError "Interface expressions are not supported in C"
generateE (TMLIR.MkExprData {}) = compilerError "Data expressions are not supported in C"
generateE (TMLIR.MkExprStruct n anns) = do
  res <- mapM (mapM generateE) anns

  let (hss, anns') = unzip $ map (\case 
          (n', (e, es)) -> (es, (n', e))
        ) res
  pure (CLang.MkExprStruct n anns', concat hss)
generateE (TMLIR.MkExprRef e _) = do
  (e', hs) <- generateE e
  pure (CLang.MkExprRef e', hs)
generateE (TMLIR.MkExprUnref e) = do
  (e', hs) <- generateE e
  pure (CLang.MkExprUnref e', hs)
generateE (TMLIR.MkExprCast e t) = do
  (e', hs) <- generateE e
  t' <- generateT Nothing t
  pure (CLang.MkExprCast e' t', hs)
generateE (TMLIR.MkExprSizeOf t) = do
  t' <- generateT Nothing t
  pure (CLang.MkExprSizeOf t', [])

generateS :: MonadIO m => TMLIR.Expression -> m (CLang.Statement, [CLang.Toplevel])
generateS (TMLIR.MkExprLet _ n t@(_ TMLIR.:->: _) e) = do
  (e', hs) <- generateE e
  t' <- generateT (Just n) t
  pure (CLang.MkStmtDeclareAssign "" t' e', hs)
generateS (TMLIR.MkExprLet _ n t e) = do
  (e', hs) <- generateE e
  t' <- generateT Nothing t
  pure (CLang.MkStmtDeclareAssign n t' e', hs)
generateS (TMLIR.MkExprWhile c e) = do
  (c', hs) <- generateE c
  (e', hs') <- generateS e
  pure (CLang.MkStmtWhile c' [e'], hs <> hs')
generateS e = do
  (e', hs) <- generateE e
  pure (CLang.MkStmtExpr e', hs)

generateUpdate :: MonadIO m => TMLIR.Update -> m (CLang.Update, [CLang.Toplevel])
generateUpdate (TMLIR.MkUpdtVariable n t) = do
  t' <- generateT Nothing t
  pure (CLang.MkUpdtVariable n t', [])
generateUpdate (TMLIR.MkUpdtField u f) = do
  (u', hs) <- generateUpdate u
  pure (CLang.MkUpdtField u' f, hs)
generateUpdate (TMLIR.MkUpdtIndex u i) = do
  (u', hs) <- generateUpdate u
  (i', hs') <- generateE i
  pure (CLang.MkUpdtIndex u' i', hs <> hs')
generateUpdate (TMLIR.MkUpdtUnref u) = do
  (u', hs) <- generateUpdate u
  pure (CLang.MkUpdtUnref u', hs)

generate :: MonadIO m => TMLIR.TMLIR "expression" -> m [CLang.Toplevel]
generate (TMLIR.MkExprInterface ann fields) = do
  fields' <- mapM (\(TMLIR.MkAnnotation n t) -> case t of
      _ TMLIR.:->: _ -> CLang.MkStructField "" <$> generateT (Just n) t
      _ -> CLang.MkStructField n <$> generateT Nothing t
    ) fields
  
  pure [CLang.MkTopStruct ann.name fields']
generate (TMLIR.MkExprData n dcs) = do
  unions <- mapM generateStruct dcs
  
  let struct = CLang.MkTopStruct n.name [
          CLang.MkStructField "header" "char*"
        , CLang.MkStructUnion mempty unions
        ]

  constructors <- concat <$> mapM (generateDC n.name) dcs

  pure $ struct : constructors

generate (TMLIR.MkExprLet _ n _ (TMLIR.MkExprLambda args ret (TMLIR.MkExprBlock es _))) = do
  (hs, body') <- mapAndUnzipM generateS es
  ret' <- generateT Nothing ret
  args' <- mapM (\(TMLIR.MkAnnotation n' t) -> (n',) <$> generateT Nothing t) args

  let hs' = case unsnoc hs of
        Just (hs'', CLang.MkStmtExpr e) -> hs'' <> [CLang.MkStmtReturn e]
        _ -> hs

  unless (n == "main") $ do
    modifyIORef' functions (<> [CLang.MkTopForwardDecl n (map snd args') ret'])

  pure $ concat body' <> [CLang.MkTopFunction n args' ret' hs']
generate (TMLIR.MkExprVariable "unit" _) = pure []
generate (TMLIR.MkExprNative ann (args TMLIR.:->: ret)) = do
  args' <- mapM (generateT Nothing) args
  ret' <- generateT Nothing ret
  pure [CLang.MkTopExtern ann.name args' ret']
generate es = compilerError $ "Cannot generate C code for expression: " <> show es

generateDC :: MonadIO m => Text -> TMLIR.DataConstructor -> m [CLang.Toplevel]
generateDC dn (TMLIR.MkDataConstructor name ts) = do
  let n' = Text.splitOn "#" name

  case n' of
    (n:_) -> do
      ts' <- mapM (generateT Nothing) ts
      let vars :: [Text] = [1..length ts] <&> \i -> "v" <> show i

      let args = zip vars ts'

      pure [CLang.MkTopFunction name args ("struct " <> CLang.varify' dn) [
        CLang.MkStmtReturn (CLang.MkExprStruct dn [
            (Just "header", CLang.MkExprLiteral (TMLIR.MkLitString n))
          , (Just n, CLang.MkExprStruct "" (zip (map Just vars) (map CLang.MkExprVariable vars)))
          ])
        ]]
    _ -> compilerError "Invalid data constructor name"
generateDC dn (TMLIR.MkDataVariable name) = do
  let n' = Text.splitOn "#" name

  case n' of
    (n:_) -> do
      pure [CLang.MkTopFunction name [] ("struct " <> CLang.varify' dn) [
        CLang.MkStmtReturn (CLang.MkExprStruct dn [
            (Just "header", CLang.MkExprLiteral (TMLIR.MkLitString n))
          , (Just n, CLang.MkExprStruct "" [])
          ])
        ]]
    _ -> compilerError "Invalid data constructor name"

generateStruct :: MonadIO m => TMLIR.DataConstructor -> m CLang.Structure
generateStruct (TMLIR.MkDataConstructor name ts) = do
  let n' = Text.splitOn "#" name
  case n' of
    (n:_) -> do
      ts' <- mapM (generateT Nothing) ts
      let vars :: [Text] = [1..length ts] <&> \i -> "v" <> show i
      let fields = zipWith CLang.MkStructField vars ts'

      pure $ CLang.MkStructStruct "" fields n
    _ -> compilerError "Invalid data constructor name"
generateStruct (TMLIR.MkDataVariable name) = do
  let n' = Text.splitOn "#" name

  case n' of
    (n:_) -> do
      modifyIORef' dataConstructors (Set.insert name)
      pure $ CLang.MkStructStruct "" [] n
    _ -> compilerError "Invalid data constructor name"

generateT :: MonadIO m => Maybe Text -> TMLIR.Type -> m Text
generateT _ (TMLIR.MkTyId "unit") = pure "void"
generateT _ (TMLIR.MkTyId "int") = pure "int"
generateT _ (TMLIR.MkTyId "char") = pure "char"
generateT _ (TMLIR.MkTyId "bool") = pure "int"
generateT _ (TMLIR.MkTyId "string") = pure "char*"
generateT _ (TMLIR.MkTyId "void") = pure "void"
generateT _ (TMLIR.MkTyId "float") = pure "float"
generateT _ (TMLIR.MkTyId "double") = pure "double"
generateT _ (TMLIR.MkTyId "long") = pure "long"
generateT _ (TMLIR.MkTyId "short") = pure "short"
generateT _ (TMLIR.MkTyId "byte") = pure "char"
generateT _ (TMLIR.MkTyActor _) = pure "struct actor_t*"
generateT _ TMLIR.MkTyAny = pure "void*"
generateT n (TMLIR.MkTyApp (TMLIR.MkTyId "ref") [t]) = do
  t' <- generateT n t
  pure $ t' <> "*"
generateT _ (TMLIR.MkTyId n) = pure $ "struct " <> CLang.varify' n
generateT _ (TMLIR.MkTyClosure _ _) = compilerError "Cannot generate C code for closure types"
generateT n (ts TMLIR.:->: t) = do
  ts' <- mapM (generateT Nothing) ts
  t' <- generateT Nothing t
  
  let ts'' = if null ts' then ["void"] else ts'

  pure $ t' <> " (*" <> fromMaybe "" n <> ")(" <> Text.intercalate ", " ts'' <> ")"
generateT _ (TMLIR.MkTyMutable t) = do
  t' <- generateT Nothing t
  pure $ t' <> "*"
generateT _ (TMLIR.MkTyApp _ _) = do
  pure "void*"
generateT n (TMLIR.MkTyVar v) = do
  tvr <- readIORef v

  case tvr of
    TMLIR.Link t -> generateT n t
    TMLIR.Unbound _ _ -> compilerError "Unbound type variable"
generateT _ (TMLIR.MkTyQuantified _) = pure "void*"

runCLangGeneration :: MonadIO m => [TMLIR.Expression] -> m [CLang.Toplevel]
runCLangGeneration es = do
  hs <- mapM generate es

  typedefs' <- readIORef typedefs
  functions' <- readIORef functions
  pure $ functions' <> typedefs' <> concat hs

pattern MkTyUnion :: [TMLIR.Type] -> TMLIR.Type
pattern MkTyUnion ts = TMLIR.MkTyApp (TMLIR.MkTyId "Union") ts

order :: [CLang.Toplevel] -> [CLang.Toplevel]
order xs = 
  let (typedefs', rest) = partition (\case 
        CLang.MkTopTypedef {} -> True
        CLang.MkTopExtern {} -> True
        CLang.MkTopEnum {} -> True
        CLang.MkTopStruct {} -> True
        CLang.MkTopUnion {} -> True 
        _ -> False) xs
    in typedefs' <> rest


unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs = case reverse xs of
  [] -> Nothing
  y:ys -> Just (reverse ys, y)