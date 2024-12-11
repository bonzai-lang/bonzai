module Language.Bonzai.Backend.TypeConversion.Conversion where

import qualified Language.Bonzai.Syntax.TMLIR as TMLIR
import qualified Language.Bonzai.Syntax.HLIR as TLIR
import Control.Monad.Result (compilerError)
import qualified Data.Map as Map
import qualified Language.Bonzai.Backend.Closure.Typed as TMLIR

convert :: MonadIO m => TLIR.TLIR "expression" -> m (TMLIR.TMLIR "expression")
convert (TLIR.MkExprLiteral l) = pure (TMLIR.MkExprLiteral l)
convert (TLIR.MkExprVariable v) = pure (TMLIR.MkExprVariable v.name (runIdentity v.value))
convert (TLIR.MkExprApplication e es t) = do
  e' <- convert e
  es' <- mapM convert es
  pure (TMLIR.MkExprApplication e' es' t.runIdentity)
convert (TLIR.MkExprLambda as ret e) = do
  let as' = map convertAnnotation as
  e' <- convert e
  pure (TMLIR.MkExprLambda as' (runIdentity ret) e')
convert (TLIR.MkExprTernary c t e ty) = do
  c' <- convert c
  t' <- convert t
  e' <- convert e
  pure (TMLIR.MkExprTernary c' t' e' ty.runIdentity)
convert (TLIR.MkExprUpdate u e) = do
  u' <- convertUpdate u
  e' <- convert e
  pure (TMLIR.MkExprUpdate u' e')
convert (TLIR.MkExprLet g v e) = do
  e' <- convert e
  pure (TMLIR.MkExprLet g v.name (runIdentity v.value) e')
convert (TLIR.MkExprBlock es t) = do
  es' <- mapM convert es
  pure (TMLIR.MkExprBlock es' t.runIdentity)
convert (TLIR.MkExprWhile c e) = do
  c' <- convert c
  e' <- convert e
  pure (TMLIR.MkExprWhile c' e')
convert (TLIR.MkExprActor t es) = do
  es' <- mapM convert es
  pure (TMLIR.MkExprActor t es')
convert (TLIR.MkExprSend e ev args _) = do
  e' <- convert e
  args' <- mapM convert args

  let substruct = TMLIR.MkExprRef (TMLIR.MkExprStruct ev (map (Nothing,) args')) (TMLIR.MkTyId ev)

  let message = TMLIR.MkExprStruct "message_t" [
          (Just "name", TMLIR.MkExprLiteral (TMLIR.MkLitString ev))
        , (Just "data", TMLIR.MkExprCast substruct TMLIR.MkTyAny)
        , (Just "type", TMLIR.MkExprLiteral (TMLIR.MkLitInt 0))
        ]

  let sendMessageTy = [TMLIR.MkTyMutable (TMLIR.MkTyId "actor_t"), TMLIR.MkTyId "message_t"] TMLIR.:->: TMLIR.MkTyUnit

  pure (TMLIR.MkExprApplication (TMLIR.MkExprVariable "send_message" sendMessageTy) [e', message] sendMessageTy)
convert (TLIR.MkExprSpawn e) = do
  e' <- convert e
  pure (TMLIR.MkExprSpawn e')
convert (TLIR.MkExprList es) = do
  es' <- mapM convert es
  pure (TMLIR.MkExprList es')
convert (TLIR.MkExprNative a t) = pure (TMLIR.MkExprNative a t)
convert (TLIR.MkExprIndex e i) = do
  e' <- convert e
  i' <- convert i
  pure (TMLIR.MkExprIndex e' i')
convert (TLIR.MkExprOn m as e) = do
  let as' = map convertAnnotation as
  e' <- convert e
  pure (TMLIR.MkExprOn m as' e')
convert (TLIR.MkExprRequire _ _) = compilerError "Cannot convert TLIR.MkExprRequire to TMLIR"
convert (TLIR.MkExprLoc e _) = convert e
convert (TLIR.MkExprInterface header anns) = do
  pure (TMLIR.MkExprInterface header anns)
convert (TLIR.MkExprData header anns) = do
  pure (TMLIR.MkExprData header (map convertDataConstructor anns))
convert (TLIR.MkExprMatch e t cs t') = do
  e' <- convert e
  let scrut = TMLIR.MkExprVariable "scrut" (runIdentity t)
  cases' <- mapM (\(p, b, _) -> (createCondition scrut p,) <$> convert b) cs

  pure (TMLIR.MkExprUnpack "scrut" t.runIdentity e' (createIfs cases' t'.runIdentity))
convert (TLIR.MkExprUnwrapLive e t) = do
  e' <- convert e
  pure (TMLIR.MkExprApplication e' [] ([] TLIR.:->: runIdentity t))
convert (TLIR.MkExprWrapLive e t) = do
  e' <- convert e
  pure (TMLIR.MkExprLambda [] t.runIdentity e')
convert (TLIR.MkExprMut e t) = do
  e' <- convert e
  pure (TMLIR.MkExprRef e' t.runIdentity)
convert (TLIR.MkExprLive {}) = compilerError "Cannot convert TLIR.MKExprLive to TMLIR"
convert (TLIR.MkExprPublic {}) = compilerError "Cannot convert TLIR.MkExprPublic to TMLIR"

convertDataConstructor :: TLIR.DataConstructor TLIR.Type -> TMLIR.DataConstructor
convertDataConstructor (TLIR.MkDataVariable n) = TMLIR.MkDataVariable n
convertDataConstructor (TLIR.MkDataConstructor n ts) = TMLIR.MkDataConstructor n ts

runTypeConversion :: MonadIO m => [TLIR.TLIR "expression"] -> m [TMLIR.TMLIR "expression"]
runTypeConversion = mapM convert

convertUpdate :: MonadIO m => TLIR.TLIR "update" -> m (TMLIR.TMLIR "update")
convertUpdate (TLIR.MkUpdtVariable v) = pure (TMLIR.MkUpdtVariable v.name (runIdentity v.value))
convertUpdate (TLIR.MkUpdtField u f) = do
  u' <- convertUpdate u
  pure (TMLIR.MkUpdtField u' f)
convertUpdate (TLIR.MkUpdtIndex u e) = do
  u' <- convertUpdate u
  e' <- convert e
  pure (TMLIR.MkUpdtIndex u' e')

convertAnnotation :: TLIR.Annotation (Identity TLIR.Type) -> TLIR.Annotation TLIR.Type
convertAnnotation (TLIR.MkAnnotation n v) = TLIR.MkAnnotation n (runIdentity v)

andTy :: TMLIR.Type
andTy = TMLIR.MkTyFun [TMLIR.MkTyBool, TMLIR.MkTyBool] TMLIR.MkTyBool

panicTy :: TMLIR.Type
panicTy = TMLIR.MkTyFun [TMLIR.MkTyString] TMLIR.MkTyUnit

createIfs
  :: [(([TMLIR.TMLIR "expression"], Map (Text, TMLIR.Type) (TMLIR.TMLIR "expression")), TMLIR.TMLIR "expression")]
  -> TMLIR.Type
  -> TMLIR.TMLIR "expression"
createIfs (((conds, maps), e): xs) t = do
  let cond = createFinalCondition conds
      lets = createLets (Map.toList maps) e t

  TMLIR.MkExprTernary cond lets (createIfs xs t) t
createIfs [] _ = TMLIR.MkExprApplication (TMLIR.MkExprVariable "panic" panicTy) [TMLIR.MkExprLiteral (TMLIR.MkLitString "non-exhaustive pattern")] panicTy

createFinalCondition :: [TMLIR.TMLIR "expression"] -> TMLIR.TMLIR "expression"
createFinalCondition [] = TMLIR.MkExprLiteral (TMLIR.MkLitInt 1)
createFinalCondition [x] = x
createFinalCondition (x : xs) = TMLIR.MkExprApplication (TMLIR.MkExprVariable "&&" andTy) [x, createFinalCondition xs] andTy

createLets
  :: [((Text, TMLIR.Type), TMLIR.TMLIR "expression")]
  -> TMLIR.TMLIR "expression"
  -> TMLIR.Type
  -> TMLIR.TMLIR "expression"
createLets xs e t = do
  let lets = map (\((n, t'), e') -> TMLIR.MkExprLet mempty n t' e') xs

  case e of
    TMLIR.MkExprBlock es t' ->
      TMLIR.MkExprBlock (lets <> es) t'

    _ ->
      TMLIR.MkExprBlock (lets <> [e]) t

idx :: TMLIR.TMLIR "expression" -> Integer -> TMLIR.TMLIR "expression"
idx e i = TMLIR.MkExprIndex e (TMLIR.MkExprLiteral (TLIR.MkLitInt i))

equalsTo :: TMLIR.TMLIR "expression" -> TMLIR.TMLIR "expression" -> TMLIR.Type -> TMLIR.Type -> TMLIR.TMLIR "expression"
equalsTo a b argTy retTy = TMLIR.MkExprApplication (TMLIR.MkExprVariable "==" (binTy argTy retTy)) [a, b] (binTy argTy retTy)

lengthTy :: TMLIR.Type -> TMLIR.Type
lengthTy ty = TMLIR.MkTyFun [ty] TMLIR.MkTyInt

binTy :: TMLIR.Type -> TMLIR.Type -> TMLIR.Type
binTy a b = TMLIR.MkTyFun [a, b] TMLIR.MkTyBool

sliceFromTy :: TMLIR.Type -> TMLIR.Type
sliceFromTy ty = TMLIR.MkTyFun [ty, TMLIR.MkTyInt] ty

sliceFrom :: TMLIR.TMLIR "expression" -> TMLIR.Type -> Integer -> TMLIR.TMLIR "expression"
sliceFrom e t i = TMLIR.MkExprApplication (TMLIR.MkExprVariable "sliceFrom" (sliceFromTy t)) [e, TMLIR.MkExprLiteral (TLIR.MkLitInt i)] (sliceFromTy t)

strcmpTy :: TMLIR.Type
strcmpTy = TMLIR.MkTyFun [TMLIR.MkTyString, TMLIR.MkTyString] TMLIR.MkTyInt

strEqualsTo :: TMLIR.TMLIR "expression" -> Text -> TMLIR.TMLIR "expression"
strEqualsTo a b = equalsTo
  (TMLIR.MkExprApplication (TMLIR.MkExprVariable "strcmp" strcmpTy) [a, TMLIR.MkExprLiteral (TLIR.MkLitString b)] strcmpTy)
  (TMLIR.MkExprLiteral (TLIR.MkLitInt 0))
  TMLIR.MkTyString
  TMLIR.MkTyInt

typeOfLit :: TLIR.Literal -> TMLIR.Type
typeOfLit TLIR.MkLitInt {} = TMLIR.MkTyInt
typeOfLit TLIR.MkLitFloat {} = TMLIR.MkTyFloat
typeOfLit TLIR.MkLitChar {} = TMLIR.MkTyChar
typeOfLit TLIR.MkLitString {} = TMLIR.MkTyString
typeOfLit TLIR.MkLitBool {} = TMLIR.MkTyBool

ors :: [TMLIR.Expression] -> TMLIR.Expression
ors [] = TMLIR.MkExprLiteral (TLIR.MkLitBool False)
ors [x] = x
ors (x : xs) = TMLIR.MkExprApplication (TMLIR.MkExprVariable "||" andTy) [x, ors xs] andTy

createCondition
  :: TMLIR.TMLIR "expression"
  -> TLIR.TLIR "pattern"
  -> ([TMLIR.TMLIR "expression"], Map (Text, TMLIR.Type) (TMLIR.TMLIR "expression"))
createCondition _ TLIR.MkPatWildcard = ([], mempty)
createCondition x (TLIR.MkPatVariable y t) = ([], Map.singleton (y, t.runIdentity) x)
createCondition x (TLIR.MkPatConstructor y xs) =
  let header = strEqualsTo (TMLIR.MkExprField x "header") y
      fields :: [Text] = [1..length xs] <&> \i -> "v" <> show i
      field = TMLIR.MkExprField x y
      (conds, maps) = unzip $ zipWith (createCondition . TMLIR.MkExprField field) fields xs
   in (header : concat conds, mconcat maps)
createCondition x (TLIR.MkPatLiteral l) =
  ([equalsTo x (TMLIR.MkExprLiteral l) (typeOfLit l) TMLIR.MkTyBool], mempty)
createCondition x (TLIR.MkPatSpecial n) = do
  let header = strEqualsTo (TMLIR.MkExprField x "header") n
  ([header], mempty)
createCondition x (TLIR.MkPatLocated y _) = do
  let (conds, maps) = createCondition x y
  (conds, maps)
createCondition x (TLIR.MkPatOr y y') = do
  let (conds, maps) = createCondition x y
  let (conds', maps') = createCondition x y'
  ([ors (conds <> conds')], maps <> maps')
createCondition _ (TLIR.MkPatCondition {}) = compilerError "Cannot convert TLIR.MkPatCondition to TMLIR"
createCondition x (TLIR.MkPatList pats slice ty) =
  let (conds, maps) =
        unzip $
          zipWith
            createCondition
            [idx x (fromIntegral i) | i <- [0 .. length pats - 1]]
            pats
      (conds', maps') = maybe mempty (createCondition (sliceFrom x ty.runIdentity patLen)) slice
      patLen = fromIntegral $ length pats
      lenCond = case slice of
        Just _ ->
          TMLIR.MkExprApplication (TMLIR.MkExprVariable ">" (binTy TLIR.MkTyInt TLIR.MkTyBool)) [
              TMLIR.MkExprApplication (TMLIR.MkExprVariable "length" (lengthTy ty.runIdentity)) [x] (lengthTy ty.runIdentity)
            , TMLIR.MkExprLiteral (TLIR.MkLitInt $ patLen - 1)
          ]
          (binTy TLIR.MkTyInt TLIR.MkTyBool)
        Nothing ->
          equalsTo
            (TMLIR.MkExprApplication (TMLIR.MkExprVariable "length" (lengthTy ty.runIdentity)) [x] (lengthTy ty.runIdentity))
            (TMLIR.MkExprLiteral (TLIR.MkLitInt (toInteger patLen)))
            TLIR.MkTyInt
            TLIR.MkTyBool
   in (lenCond : concat conds <> conds', mconcat maps <> maps')