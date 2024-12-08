{-# LANGUAGE LambdaCase #-}
module Language.Bonzai.Backend.Monomorphization.Conversion where

import qualified Language.Bonzai.Syntax.TMLIR as TMLIR
import qualified GHC.IO as IO
import qualified Data.Map as Map
import qualified Data.Text as Text
import Control.Monad.Result (compilerError)

{-# NOINLINE replacedFunctions #-}
replacedFunctions :: IORef (Map (Text, TMLIR.Type) Text)
replacedFunctions = IO.unsafePerformIO $ newIORef Map.empty

{-# NOINLINE output #-}
output :: IORef [TMLIR.Expression]
output = IO.unsafePerformIO $ newIORef []

{-# NOINLINE functions #-}
functions :: IORef (Map Text (TMLIR.Type, TMLIR.Expression))
functions = IO.unsafePerformIO $ newIORef Map.empty

{-# NOINLINE replacedDataConstructors #-}
replacedDataConstructors :: IORef (Map (Text, [TMLIR.Type]) (Map Text Text))
replacedDataConstructors = IO.unsafePerformIO $ newIORef Map.empty

replacedDatatypes :: IORef (Map (Text, [TMLIR.Type]) Text)
replacedDatatypes = IO.unsafePerformIO $ newIORef Map.empty

datatypes :: IORef (Map Text ([TMLIR.QuVar], [TMLIR.DataConstructor]))
datatypes = IO.unsafePerformIO $ newIORef Map.empty

typeToSubst :: MonadIO m => TMLIR.Type -> TMLIR.Type -> m (Map Text TMLIR.Type)
typeToSubst (TMLIR.MkTyVar name) t = do
  tvar <- readIORef name

  case tvar of
    TMLIR.Link t' -> typeToSubst t' t
    TMLIR.Unbound name' _ -> return $ Map.singleton name' t
typeToSubst t (TMLIR.MkTyVar name) = do
  tvar <- readIORef name

  case tvar of
    TMLIR.Link t' -> typeToSubst t t'
    TMLIR.Unbound name' _ -> return $ Map.singleton name' t
typeToSubst (TMLIR.MkTyApp t1 t2) (TMLIR.MkTyApp t1' t2') = do
  s1 <- typeToSubst t1 t1'
  s2 <- Map.unions <$> zipWithM typeToSubst t2 t2'

  return $ s1 <> s2
typeToSubst (TMLIR.MkTyId name) (TMLIR.MkTyId name') | name == name' = return Map.empty
typeToSubst (TMLIR.MkTyQuantified name) t = return $ Map.singleton name t
typeToSubst t (TMLIR.MkTyQuantified name) = return $ Map.singleton name t
typeToSubst t (TMLIR.MkTyId name) = return $ Map.singleton name t
typeToSubst (TMLIR.MkTyId name) t = return $ Map.singleton name t

getDataType :: TMLIR.Type -> Maybe (Text, [TMLIR.Type])
getDataType (_ TMLIR.:->: ret) = getDataType ret
getDataType (TMLIR.MkTyApp (TMLIR.MkTyId n) ts) = Just (n, ts)
getDataType (TMLIR.MkTyId n) = Just (n, [])
getDataType _ = Nothing

monomorphize :: MonadIO m => TMLIR.Expression -> m TMLIR.Expression
monomorphize (TMLIR.MkExprVariable name t) = do
  rfs <- readIORef replacedFunctions
  fs <- readIORef functions

  case Map.lookup (name, t) rfs of
    Just name' -> return $ TMLIR.MkExprVariable name' t
    Nothing -> case Map.lookup name fs of
      Just (t', expr) -> do
        subst <- typeToSubst t t'

        newTy <- applyT t subst

        let name' = name <> "_" <> Text.intercalate "_" (map show $ Map.elems subst)
        modifyIORef' replacedFunctions (Map.insert (name, newTy) name')

        expr' <- applyE expr subst
        expr'' <- monomorphize expr'

        modifyIORef' output (<> [TMLIR.MkExprLet mempty name' newTy expr''])

        return $ TMLIR.MkExprVariable name' newTy
      Nothing -> do
        t' <- TMLIR.simplify t
        let res = getDataType t'
        
        case res of
          Just (n, ts) -> do
            rdcs <- readIORef replacedDataConstructors

            case Map.lookup (n, ts) rdcs of
              Just m -> case Map.lookup name m of
                Just name' -> return $ TMLIR.MkExprVariable name' t
                Nothing -> return $ TMLIR.MkExprVariable name t
              Nothing -> return $ TMLIR.MkExprVariable name t
          Nothing -> return $ TMLIR.MkExprVariable name t
monomorphize (TMLIR.MkExprLet gens name t expr) = do
  if null gens then do
    expr' <- monomorphize expr
    expr'' <- applyE expr' mempty
    return $ TMLIR.MkExprLet gens name t expr''
  else do
    modifyIORef' functions (Map.insert name (t, expr))
    return $ TMLIR.MkExprVariable "unit" TMLIR.MkTyUnit
monomorphize (TMLIR.MkExprWhile cond expr) = do
  cond' <- monomorphize cond
  expr' <- monomorphize expr
  return $ TMLIR.MkExprWhile cond' expr'
monomorphize (TMLIR.MkExprActor t exprs) = do
  exprs' <- mapM monomorphize exprs
  return $ TMLIR.MkExprActor t exprs'
monomorphize (TMLIR.MkExprSend expr name exprs t) = do
  expr' <- monomorphize expr
  exprs' <- mapM monomorphize exprs
  return $ TMLIR.MkExprSend expr' name exprs' t
monomorphize (TMLIR.MkExprSpawn expr) = do
  expr' <- monomorphize expr
  return $ TMLIR.MkExprSpawn expr'
monomorphize (TMLIR.MkExprList exprs) = do
  exprs' <- mapM monomorphize exprs
  return $ TMLIR.MkExprList exprs'
monomorphize (TMLIR.MkExprNative ann t) = return $ TMLIR.MkExprNative ann t
monomorphize (TMLIR.MkExprIndex expr index) = do
  expr' <- monomorphize expr
  index' <- monomorphize index
  return $ TMLIR.MkExprIndex expr' index'
monomorphize (TMLIR.MkExprOn name anns expr) = do
  expr' <- monomorphize expr
  return $ TMLIR.MkExprOn name anns expr'
monomorphize (TMLIR.MkExprApplication expr exprs t) = do
  expr' <- monomorphize expr
  exprs' <- mapM monomorphize exprs
  return $ TMLIR.MkExprApplication expr' exprs' t
monomorphize (TMLIR.MkExprLambda anns t expr) = do
  expr' <- monomorphize expr
  return $ TMLIR.MkExprLambda anns t expr'
monomorphize (TMLIR.MkExprTernary cond true false t) = do
  cond' <- monomorphize cond
  true' <- monomorphize true
  false' <- monomorphize false
  return $ TMLIR.MkExprTernary cond' true' false' t
monomorphize (TMLIR.MkExprField expr name) = do
  expr' <- monomorphize expr
  return $ TMLIR.MkExprField expr' name
monomorphize (TMLIR.MkExprUpdate update expr) = do
  update' <- monomorphizeUpdate update
  expr' <- monomorphize expr
  return $ TMLIR.MkExprUpdate update' expr'
monomorphize (TMLIR.MkExprBlock exprs t) = do
  exprs' <- mapM monomorphize exprs
  return $ TMLIR.MkExprBlock exprs' t
monomorphize (TMLIR.MkExprData ann cons) = do
  if null ann.value then do
    return $ TMLIR.MkExprData ann cons
  else do
    modifyIORef' datatypes (Map.insert ann.name (ann.value, cons))

    return $ TMLIR.MkExprVariable "unit" TMLIR.MkTyUnit
monomorphize (TMLIR.MkExprInterface ann cons) = do
  return $ TMLIR.MkExprInterface ann cons
monomorphize (TMLIR.MkExprLiteral l) = return $ TMLIR.MkExprLiteral l
monomorphize (TMLIR.MkExprUnpack name t expr expr') = do
  expr'' <- monomorphize expr
  expr''' <- monomorphize expr'
  return $ TMLIR.MkExprUnpack name t expr'' expr'''
monomorphize (TMLIR.MkExprStruct n anns) = do
  anns' <- mapM (mapM monomorphize) anns
  return $ TMLIR.MkExprStruct n anns'
monomorphize (TMLIR.MkExprRef expr t) = do
  expr' <- monomorphize expr
  return $ TMLIR.MkExprRef expr' t
monomorphize (TMLIR.MkExprUnref expr) = do
  expr' <- monomorphize expr
  return $ TMLIR.MkExprUnref expr'
monomorphize (TMLIR.MkExprCast expr t) = do
  expr' <- monomorphize expr
  return $ TMLIR.MkExprCast expr' t
monomorphize (TMLIR.MkExprSizeOf t) = return $ TMLIR.MkExprSizeOf t

monomorphizeUpdate :: MonadIO m => TMLIR.Update -> m TMLIR.Update
monomorphizeUpdate (TMLIR.MkUpdtVariable name t) = return $ TMLIR.MkUpdtVariable name t
monomorphizeUpdate (TMLIR.MkUpdtField update name) = do
  update' <- monomorphizeUpdate update
  return $ TMLIR.MkUpdtField update' name
monomorphizeUpdate (TMLIR.MkUpdtIndex update expr) = do
  update' <- monomorphizeUpdate update
  expr' <- monomorphize expr
  return $ TMLIR.MkUpdtIndex update' expr'
monomorphizeUpdate (TMLIR.MkUpdtUnref update) = do
  update' <- monomorphizeUpdate update
  return $ TMLIR.MkUpdtUnref update'

applyT :: MonadIO m => TMLIR.Type -> Map Text TMLIR.Type -> m TMLIR.Type
applyT (TMLIR.MkTyVar name) subst = do
  tvr <- readIORef name

  case tvr of
    TMLIR.Link t -> applyT t subst
    TMLIR.Unbound name' _ -> case Map.lookup name' subst of
      Just t -> pure t
      Nothing -> pure $ TMLIR.MkTyVar name
applyT (TMLIR.MkTyApp (TMLIR.MkTyId n) ts) subst = do
  ts' <- mapM (`applyT` subst) ts

  rdts <- readIORef replacedDatatypes
  dts <- readIORef datatypes
  case Map.lookup (n, ts') rdts of
    Just n' -> return $ TMLIR.MkTyId n'

    Nothing -> case Map.lookup n dts of
      Just (qvars, dcs) -> do
        let subst' = Map.fromList (zip qvars ts)

        let name = n <> "_" <> Text.intercalate "_" (map show ts')
        modifyIORef' replacedDatatypes (Map.insert (n, ts') name)

        dcs' <- mapM (applyDC (subst <> subst') ts') dcs
        modifyIORef' output (<> [TMLIR.MkExprData (TMLIR.MkAnnotation name []) dcs'])

        let pairs = zipWith (\x y -> case (x, y) of
                (TMLIR.MkDataConstructor name' _, TMLIR.MkDataConstructor name'' _) -> (name', name'')
                (TMLIR.MkDataVariable name', TMLIR.MkDataVariable name'') -> (name', name'')
                _ -> compilerError "Cannot replace data constructor with data variable"
              ) dcs dcs'

        modifyIORef' replacedDataConstructors (Map.insert (n, ts') (Map.fromList pairs))
        modifyIORef' replacedDataConstructors (Map.insert (name, []) (Map.fromList pairs))

        return $ TMLIR.MkTyId name

      _ -> do
        id' <- applyT (TMLIR.MkTyId n) subst

        pure $ TMLIR.MkTyApp id' ts'
applyT (TMLIR.MkTyApp t1 t2) subst = TMLIR.MkTyApp <$>applyT t1 subst <*> mapM (`applyT` subst) t2
applyT (TMLIR.MkTyId name) subst = case Map.lookup name subst of
  Just t -> pure t
  Nothing -> pure $ TMLIR.MkTyId name
applyT (TMLIR.MkTyQuantified name) subst = case Map.lookup name subst of
  Just t -> pure t
  Nothing -> pure $ TMLIR.MkTyQuantified name

applyDC :: MonadIO m => Map Text TMLIR.Type -> [TMLIR.Type] -> TMLIR.DataConstructor -> m TMLIR.DataConstructor
applyDC subst tvars (TMLIR.MkDataConstructor name ts) = do
  ts' <- mapM (`applyT` subst) ts

  let newName = name <> "#" <> Text.intercalate "_" (map show tvars)

  return $ TMLIR.MkDataConstructor newName ts'
applyDC _ tvars (TMLIR.MkDataVariable name) = do
  let newName = name <> "#" <> Text.intercalate "_" (map show tvars)

  return $ TMLIR.MkDataVariable newName

applyE :: MonadIO m => TMLIR.Expression -> Map Text TMLIR.Type -> m TMLIR.Expression
applyE (TMLIR.MkExprVariable name t) subst = do
  t' <- TMLIR.simplify t >> applyT t subst
  return $ TMLIR.MkExprVariable name t'
applyE (TMLIR.MkExprLet gens name t expr) subst = do
  expr' <- applyE expr subst
  t' <- TMLIR.simplify t >> applyT t subst
  return $ TMLIR.MkExprLet gens name t' expr'
applyE (TMLIR.MkExprWhile cond expr) subst = do
  cond' <- applyE cond subst
  expr' <- applyE expr subst
  return $ TMLIR.MkExprWhile cond' expr'
applyE (TMLIR.MkExprActor t exprs) subst = do
  t' <- TMLIR.simplify t >> applyT t subst
  exprs' <- mapM (`applyE` subst) exprs
  return $ TMLIR.MkExprActor t' exprs'
applyE (TMLIR.MkExprSend expr name exprs t) subst = do
  t' <- TMLIR.simplify t >> applyT t subst
  expr' <- applyE expr subst
  exprs' <- mapM (`applyE` subst) exprs
  return $ TMLIR.MkExprSend expr' name exprs' t'
applyE (TMLIR.MkExprSpawn expr) subst = do
  expr' <- applyE expr subst
  return $ TMLIR.MkExprSpawn expr'
applyE (TMLIR.MkExprList exprs) subst = do
  exprs' <- mapM (`applyE` subst) exprs
  return $ TMLIR.MkExprList exprs'
applyE (TMLIR.MkExprNative ann t) _ = return $ TMLIR.MkExprNative ann t
applyE (TMLIR.MkExprIndex expr index) subst = do
  expr' <- applyE expr subst
  index' <- applyE index subst
  return $ TMLIR.MkExprIndex expr' index'
applyE (TMLIR.MkExprOn name anns expr) subst = do
  args' <- mapM (traverse (`applyT` subst)) anns
  expr' <- applyE expr subst
  return $ TMLIR.MkExprOn name args' expr'
applyE (TMLIR.MkExprApplication expr exprs t) subst = do
  expr' <- applyE expr subst
  t' <- TMLIR.simplify t >> applyT t subst
  exprs' <- mapM (`applyE` subst) exprs
  return $ TMLIR.MkExprApplication expr' exprs' t'
applyE (TMLIR.MkExprLambda anns t expr) subst = do
  args' <- mapM (traverse (TMLIR.simplify >=> (`applyT` subst))) anns

  expr' <- applyE expr subst
  t' <- TMLIR.simplify t >> applyT t subst
  return $ TMLIR.MkExprLambda args' t' expr'
applyE (TMLIR.MkExprTernary cond true false t) subst = do
  cond' <- applyE cond subst
  true' <- applyE true subst
  false' <- applyE false subst
  t' <- TMLIR.simplify t >> applyT t subst
  return $ TMLIR.MkExprTernary cond' true' false' t'
applyE (TMLIR.MkExprField expr name) subst = do
  expr' <- applyE expr subst
  return $ TMLIR.MkExprField expr' name
applyE (TMLIR.MkExprUpdate update expr) subst = do
  update' <- applyUpdate update subst
  expr' <- applyE expr subst
  return $ TMLIR.MkExprUpdate update' expr'
applyE (TMLIR.MkExprBlock exprs t) subst = do
  exprs' <- mapM (`applyE` subst) exprs
  t' <- TMLIR.simplify t >> applyT t subst

  return $ TMLIR.MkExprBlock exprs' t'
applyE (TMLIR.MkExprData ann cons) _ = return $ TMLIR.MkExprData ann cons
applyE (TMLIR.MkExprInterface ann cons) _ = return $ TMLIR.MkExprInterface ann cons
applyE (TMLIR.MkExprLiteral l) _ = return $ TMLIR.MkExprLiteral l
applyE (TMLIR.MkExprUnpack name t expr expr') subst = do
  t' <- TMLIR.simplify t >> applyT t subst
  expr'' <- applyE expr subst
  expr''' <- applyE expr' subst
  return $ TMLIR.MkExprUnpack name t' expr'' expr'''
applyE (TMLIR.MkExprStruct n anns) subst = do
  anns' <- mapM (mapM (`applyE` subst)) anns
  return $ TMLIR.MkExprStruct n anns'
applyE (TMLIR.MkExprRef expr t) subst = do
  expr' <- applyE expr subst
  t' <- TMLIR.simplify t >> applyT t subst
  return $ TMLIR.MkExprRef expr' t'
applyE (TMLIR.MkExprUnref expr) subst = do
  expr' <- applyE expr subst
  return $ TMLIR.MkExprUnref expr'
applyE (TMLIR.MkExprCast expr t) subst = do
  expr' <- applyE expr subst
  t' <- TMLIR.simplify t >> applyT t subst
  return $ TMLIR.MkExprCast expr' t'
applyE (TMLIR.MkExprSizeOf t) subst = do
  t' <- TMLIR.simplify t >> applyT t subst
  return $ TMLIR.MkExprSizeOf t'

applyUpdate :: MonadIO m => TMLIR.Update -> Map Text TMLIR.Type -> m TMLIR.Update
applyUpdate (TMLIR.MkUpdtVariable name t) subst = do
  t' <- TMLIR.simplify t >> applyT t subst
  return $ TMLIR.MkUpdtVariable name t'
applyUpdate (TMLIR.MkUpdtField update name) subst = do
  update' <- applyUpdate update subst
  return $ TMLIR.MkUpdtField update' name
applyUpdate (TMLIR.MkUpdtIndex update expr) subst = do
  update' <- applyUpdate update subst
  expr' <- applyE expr subst
  return $ TMLIR.MkUpdtIndex update' expr'
applyUpdate (TMLIR.MkUpdtUnref update) subst = do
  update' <- applyUpdate update subst
  return $ TMLIR.MkUpdtUnref update'

runMonomorphization :: MonadIO m => [TMLIR.Expression] -> m [TMLIR.Expression]
runMonomorphization exprs = do
  writeIORef output []
  writeIORef functions Map.empty
  writeIORef replacedFunctions Map.empty

  exprs' <- mapM monomorphize exprs
  output' <- readIORef output

  return $ output' <> exprs'