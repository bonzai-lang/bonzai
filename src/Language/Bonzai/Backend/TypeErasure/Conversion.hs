module Language.Bonzai.Backend.TypeErasure.Conversion where

import qualified Language.Bonzai.Syntax.HLIR as HLIR
import qualified Language.Bonzai.Syntax.MLIR as MLIR
import Control.Monad.Result (compilerError)
import qualified Data.Map as Map
import Language.Bonzai.Backend.Closure.Free (Substitutable(substitute))
import qualified Data.List as List

convert :: HLIR.TLIR "expression" -> MLIR.MLIR "expression"
convert (HLIR.MkExprLiteral l) = MLIR.MkExprLiteral l
convert (HLIR.MkExprVariable a) = MLIR.MkExprVariable a.name
convert (HLIR.MkExprApplication f args) = MLIR.MkExprApplication (convert f) (map convert args)
convert (HLIR.MkExprLet _ ann e) = MLIR.MkExprLet ann.name (convert e)
convert (HLIR.MkExprBlock es) = MLIR.MkExprBlock (map convert es)
convert (HLIR.MkExprActor _ es) = MLIR.MkExprEvent (map convert es)
convert (HLIR.MkExprOn ev as e) = MLIR.MkExprOn ev (map (.name) as) (convert e)
convert (HLIR.MkExprSend e ev es _) = MLIR.MkExprSend (convert e) ev (map convert es)
convert (HLIR.MkExprRequire _) = compilerError "require is not supported in MLIR"
convert (HLIR.MkExprLoc e p) = MLIR.MkExprLoc p (convert e)
convert (HLIR.MkExprSpawn e) = MLIR.MkExprSpawn (convert e)
convert (HLIR.MkExprLambda as _ e) = MLIR.MkExprLambda (map (.name) as) (convert e)
convert (HLIR.MkExprTernary c t e) = MLIR.MkExprTernary (convert c) (convert t) (convert e)
convert (HLIR.MkExprUpdate u e) = MLIR.MkExprUpdate (convertUpdate u) (convert e)
convert (HLIR.MkExprList es) = MLIR.MkExprList (map convert es)
convert (HLIR.MkExprNative n ty) = MLIR.MkExprNative n ty
convert (HLIR.MkExprMut a e) = MLIR.MkExprMut a.name (convert e)
convert (HLIR.MkExprInterface {}) = MLIR.MkExprLiteral (HLIR.MkLitInt 0)
convert (HLIR.MkExprWhile c e) = MLIR.MkExprWhile (convert c) (convert e)
convert (HLIR.MkExprIndex e e') = MLIR.MkExprIndex (convert e) (convert e')
convert (HLIR.MkExprMatch e cs) = do
  let e' = convert e
  let scrut = MLIR.MkExprVariable "scrut"
  let cases' = map (\(p, e', _) -> (createCondition scrut p, convert e')) cs

  MLIR.MkExprUnpack "scrut" e' (createIfs cases')
convert (HLIR.MkExprUnwrapLive e) = MLIR.MkExprApplication (convert e) []
convert (HLIR.MkExprWrapLive e) = MLIR.MkExprLambda [] (convert e)
convert _ = compilerError "unimplemented"

createFunction :: Text -> HLIR.TLIR "data" -> MLIR.MLIR "expression"
createFunction typeName (HLIR.MkDataConstructor name args) = do
  let args' = ["x" <> show i | i <- [(0 :: Int) .. length args - 1]]
  let body = MLIR.MkExprList $ [ MLIR.MkExprSpecial, MLIR.MkExprLiteral (MLIR.MkLitString typeName), MLIR.MkExprLiteral (MLIR.MkLitString name)] <> map MLIR.MkExprVariable args'

  MLIR.MkExprFunction name args' body
createFunction typeName (HLIR.MkDataVariable name) = do
  let body = MLIR.MkExprList [MLIR.MkExprSpecial, MLIR.MkExprLiteral (MLIR.MkLitString typeName), MLIR.MkExprLiteral (MLIR.MkLitString name)]

  MLIR.MkExprLet name body

createIfs
  :: [(([MLIR.MLIR "expression"], Map Text (MLIR.MLIR "expression")), MLIR.MLIR "expression")]
  -> MLIR.MLIR "expression"
createIfs (((conds, maps), e ): xs) = do
  let cond = createFinalCondition conds
      lets = createLets (Map.toList maps) e

  MLIR.MkExprTernary cond lets (createIfs xs)
createIfs [] = MLIR.MkExprApplication (MLIR.MkExprVariable "panic") [MLIR.MkExprLiteral (HLIR.MkLitString "non-exhaustive pattern")]

createFinalCondition :: [MLIR.MLIR "expression"] -> MLIR.MLIR "expression"
createFinalCondition [] = MLIR.MkExprLiteral (MLIR.MkLitInt 1)
createFinalCondition [x] = x
createFinalCondition (x : xs) = MLIR.MkExprApplication (MLIR.MkExprVariable "&&") [x, createFinalCondition xs]

createLets
  :: [(Text, MLIR.MLIR "expression")]
  -> MLIR.MLIR "expression"
  -> MLIR.MLIR "expression"
createLets xs e = do
  let lets = map (uncurry MLIR.MkExprLet) xs

  case e of
    MLIR.MkExprBlock es ->
      MLIR.MkExprBlock (lets <> es)

    _ ->
      MLIR.MkExprBlock (lets <> [e])

convertUpdate :: HLIR.TLIR "update" -> MLIR.MLIR "update"
convertUpdate (HLIR.MkUpdtVariable a) = MLIR.MkUpdtVariable a.name
convertUpdate (HLIR.MkUpdtField u f) = MLIR.MkUpdtField (convertUpdate u) f
convertUpdate (HLIR.MkUpdtIndex u e) = MLIR.MkUpdtIndex (convertUpdate u) (convert e)

eraseTypes :: [HLIR.TLIR "expression"] -> [MLIR.MLIR "expression"]
eraseTypes (HLIR.MkExprData ann cs : xs) = map (createFunction ann.name) cs <> eraseTypes xs
eraseTypes (HLIR.MkExprLoc e p : xs) = (MLIR.MkExprLoc p <$> eraseTypes [e]) <> eraseTypes xs
eraseTypes (x : xs) = convert x : eraseTypes xs
eraseTypes [] = []

idx :: MLIR.MLIR "expression" -> Integer -> MLIR.MLIR "expression"
idx e i = MLIR.MkExprIndex e (MLIR.MkExprLiteral (HLIR.MkLitInt i))

equalsTo :: MLIR.MLIR "expression" -> MLIR.MLIR "expression" -> MLIR.MLIR "expression"
equalsTo a b = MLIR.MkExprApplication (MLIR.MkExprVariable "==") [a, b]

sliceFrom :: MLIR.MLIR "expression" -> Integer -> MLIR.MLIR "expression"
sliceFrom e i = MLIR.MkExprApplication (MLIR.MkExprVariable "sliceFrom") [e, MLIR.MkExprLiteral (HLIR.MkLitInt i)]

createCondition
  :: MLIR.MLIR "expression"
  -> HLIR.TLIR "pattern"
  -> ([MLIR.MLIR "expression"], Map Text (MLIR.MLIR "expression"))
createCondition _ HLIR.MkPatWildcard = ([], mempty)
createCondition x (HLIR.MkPatVariable y _) = ([], Map.singleton y x)
createCondition x (HLIR.MkPatConstructor y xs) =
  let spc = equalsTo (idx x 0) MLIR.MkExprSpecial
      cons = equalsTo (idx x 2) (MLIR.MkExprLiteral (MLIR.MkLitString y))
      (conds, maps) = unzip $ zipWith (createCondition . idx x) [3 ..] xs
   in (spc : cons : concat conds, mconcat maps)
createCondition x (HLIR.MkPatLiteral l) =
  ([equalsTo x (MLIR.MkExprLiteral l)], mempty)
createCondition x (HLIR.MkPatSpecial n) = do
  let spc = equalsTo (idx x 0) MLIR.MkExprSpecial
  let cons = equalsTo (idx x 2) (MLIR.MkExprLiteral (MLIR.MkLitString n))
  ([spc, cons], mempty)
createCondition x (HLIR.MkPatLocated y p) = do
  let (conds, maps) = createCondition x y
  (MLIR.MkExprLoc p <$> conds, maps)
createCondition x (HLIR.MkPatOr y y') = do
  let (conds, maps) = createCondition x y
  let (conds', maps') = createCondition x y'
  ([MLIR.MkExprApplication (MLIR.MkExprVariable "||") [MLIR.MkExprBlock conds, MLIR.MkExprBlock conds']], maps <> maps')
createCondition x (HLIR.MkPatCondition e y) = do
  let (conds, maps) = createCondition x y
  let e' = convert e
  ([MLIR.MkExprApplication (MLIR.MkExprVariable "&&") [MLIR.MkExprBlock conds, susbstituteMap (Map.toList maps) e']], maps)
createCondition x (HLIR.MkPatList pats slice) =
  let (conds, maps) =
        unzip $
          zipWith
            createCondition
            [idx x (fromIntegral i) | i <- [0 .. length pats - 1]]
            pats
      (conds', maps') = maybe mempty (createCondition (sliceFrom x patLen)) slice
      patLen = fromIntegral $ length pats
      lenCond = case slice of
        Just _ ->
          MLIR.MkExprApplication (MLIR.MkExprVariable ">") [
              MLIR.MkExprApplication (MLIR.MkExprVariable "length") [x]
            , MLIR.MkExprLiteral (HLIR.MkLitInt $ patLen - 1)
          ]
        Nothing ->
          equalsTo
            (MLIR.MkExprApplication (MLIR.MkExprVariable "length") [x])
            (MLIR.MkExprLiteral (HLIR.MkLitInt (toInteger patLen)))
   in (lenCond : concat conds <> conds', mconcat maps <> maps')

susbstituteMap :: [(Text, MLIR.MLIR "expression")] -> MLIR.MLIR "expression" -> MLIR.MLIR "expression"
susbstituteMap xs e = List.foldl (flip substitute) e xs