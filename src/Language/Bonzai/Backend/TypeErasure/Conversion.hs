module Language.Bonzai.Backend.TypeErasure.Conversion where

import Control.Monad.Result (compilerError)
import Data.List qualified as List
import Data.Map qualified as Map
import Language.Bonzai.Backend.Closure.Free (Substitutable (substitute))
import Language.Bonzai.Syntax.HLIR qualified as HLIR
import Language.Bonzai.Syntax.MLIR qualified as MLIR

--  | MLIR CONVERSION

-- |
-- | This pass converts the HLIR to MLIR by erasing all type information.
-- | It also converts datatypes and their constructors to functions that
-- | return a list of strings representing the type and constructor name.
-- |
-- | For example, the following datatype:
-- |
-- | data Maybe a = Just a | Nothing
-- |
-- | will be converted to the following functions:
-- |
-- | let Just x = [Special, "Maybe", "Just", x]
-- | let Nothing = [Special, "Maybe", "Nothing"]
-- |
-- | The conversion also includes the conversion of pattern matches to if-else
-- | expressions. This is done by converting each pattern match to a series of
-- | if-else expressions that check the conditions of the pattern match.
-- |
-- | For example, the following pattern match:
-- |
-- | case x of
-- |   Just y -> y
-- |   Nothing -> 0
-- |
-- | will be converted to the following if-else expression:
-- |
-- | if x[0] == Special && x[1] == "Maybe" && x[2] == "Just"
-- |   y
-- | else if x[0] == Special && x[1] == "Maybe" && x[2] == "Nothing"
-- |   0
-- | else
-- |   panic "non-exhaustive pattern"
convert :: HLIR.TLIR "expression" -> MLIR.MLIR "expression"
convert (HLIR.MkExprLiteral l) = MLIR.MkExprLiteral l
convert (HLIR.MkExprApplication var [x, y, _])
  | isVariable var,
    name <- getVariable var,
    name `elem` operators =
      MLIR.MkExprBinary name (convert x) (convert y)
convert (HLIR.MkExprVariable a) = MLIR.MkExprVariable a.name
convert (HLIR.MkExprApplication f args) = MLIR.MkExprApplication (convert f) (map convert args)
convert (HLIR.MkExprLet _ (Left ann) e b) | Just constrs <- getData e = do
  let funs = map (createFunction' (Just ann.name)) constrs
  let record = MLIR.MkExprRecord (Map.fromList funs)

  let b' = convert b

  MLIR.MkExprUnpack ann.name record b'
convert (HLIR.MkExprLet _ (Left ann) e b) = case convert b of
  MLIR.MkExprVariable "unit" -> MLIR.MkExprLet ann.name (convert e)
  b' -> MLIR.MkExprUnpack ann.name (convert e) b'
convert (HLIR.MkExprLet _ (Right pat) e b) | isUnit b = do
  let e' = convert e

  let var = MLIR.MkExprVariable "scrut"

  let (conds, maps) = createCondition var pat

  let lets = Map.toList maps

  let vars
        | null conds = MLIR.MkExprUnpack "scrut" e' (createIfs' conds maps)
        | otherwise = foldl' (\acc (k, _) -> MLIR.MkExprUnpack k MLIR.MkExprSpecial acc) (MLIR.MkExprUnpack "scrut" e' (createIfs' conds maps)) lets

  vars
convert (HLIR.MkExprLet _ (Right pat) e b) = do
  let e' = convert e
  let var = MLIR.MkExprVariable "scrut"
  let (conds, maps) = createCondition var pat
  let b' = convert b

  let lets = Map.toList maps
  
  let vars
        | null conds = MLIR.MkExprUnpack "scrut" e' (createIfs' conds maps)
        | otherwise = foldl' (\acc (k, _) -> MLIR.MkExprUnpack k MLIR.MkExprSpecial acc) (MLIR.MkExprUnpack "scrut" e' (createIfs' conds maps)) lets

  MLIR.MkExprUnpack "_" vars b'
convert (HLIR.MkExprBlock es) = MLIR.MkExprBlock (map convert es)
convert (HLIR.MkExprRequire _ _) = compilerError "require is not supported in MLIR"
convert (HLIR.MkExprLoc e p) = MLIR.MkExprLoc p (convert e)
convert (HLIR.MkExprLambda as _ e) = MLIR.MkExprLambda (map (.name) as) (convert e)
convert (HLIR.MkExprTernary c t e) = MLIR.MkExprTernary (convert c) (convert t) (convert e)
convert (HLIR.MkExprUpdate u e) = MLIR.MkExprUpdate (convertUpdate u) (convert e)
convert (HLIR.MkExprList es) = MLIR.MkExprList (map convert es)
convert (HLIR.MkExprNative n ty) = MLIR.MkExprNative n ty
convert (HLIR.MkExprInterface {}) = MLIR.MkExprLiteral (HLIR.MkLitInt 0)
convert (HLIR.MkExprWhile c e) = MLIR.MkExprWhile (convert c) (convert e)
convert (HLIR.MkExprIndex e e') = MLIR.MkExprIndex (convert e) (convert e')
convert (HLIR.MkExprMatch e cs) = do
  let e' = convert e
  let scrut = MLIR.MkExprVariable "scrut"
  let cases' = map (\(p, b, _) -> (createCondition scrut p, convert b)) cs

  MLIR.MkExprUnpack "scrut" e' (createIfs cases')
convert (HLIR.MkExprMut e) = MLIR.MkExprMut (convert e)
convert (HLIR.MkExprRecordExtension e k opt v) = do
  let (xs, e') = reduceObj (HLIR.MkExprRecordExtension e k opt v)

  let obj = MLIR.MkExprRecord (Map.fromList xs)

  case e' of
    Just (MLIR.MkExprRecord m) | m == mempty -> obj
    Just e'' -> MLIR.MkExprBinary "+" obj e''
    Nothing -> obj
convert (HLIR.MkExprRecordAccess e k) = do
  let e' = convert e

  MLIR.MkExprRecordAccess e' k
convert HLIR.MkExprRecordEmpty = MLIR.MkExprRecord mempty
convert (HLIR.MkExprSingleIf c t) = MLIR.MkExprSingleIf (convert c) (convert t)
convert (HLIR.MkExprPublic e) = convert e
convert HLIR.MkExprBreak = MLIR.MkExprBreak
convert HLIR.MkExprContinue = MLIR.MkExprContinue
convert (HLIR.MkExprReturn e) = MLIR.MkExprReturn (convert e)
convert (HLIR.MkExprData cs) = do
  let funs = map (createFunction' Nothing) cs
  MLIR.MkExprRecord (Map.fromList funs)
convert (HLIR.MkExprSpawn e) = MLIR.MkExprSpawn (convert e)

unit :: MLIR.Expression 
unit = MLIR.MkExprVariable "unit"

createIfs' :: [MLIR.MLIR "expression"] -> Map Text MLIR.Expression -> MLIR.MLIR "expression"
createIfs' [] m = foldl' (\acc (k, v) -> MLIR.MkExprUnpack k v acc) unit (Map.toList m) 
createIfs' (x : xs) m = do
  MLIR.MkExprTernary x (createIfs' xs m) (MLIR.MkExprApplication (MLIR.MkExprVariable "panic") [MLIR.MkExprLiteral (HLIR.MkLitString "non-exhaustive pattern")])

isUnit :: HLIR.TLIR "expression" -> Bool
isUnit (HLIR.MkExprVariable a) = a.name == "unit"
isUnit (HLIR.MkExprLoc e _) = isUnit e
isUnit _ = False

toList' :: [(Text, MLIR.MLIR "expression")] -> [MLIR.MLIR "expression"]
toList' [] = []
toList' ((k, v) : xs) = MLIR.MkExprLiteral (HLIR.MkLitString k) : v : toList' xs

reduceObj :: HLIR.TLIR "expression" -> ([(Text, MLIR.MLIR "expression")], Maybe (MLIR.MLIR "expression"))
reduceObj (HLIR.MkExprRecordExtension e k _ dat) | Just constrs <- getData dat = do
  let funs = map (createFunction' (Just k)) constrs
  let record = MLIR.MkExprRecord (Map.fromList funs)
  let (xs, e') = reduceObj e

  ((k, record) : xs, e')
reduceObj (HLIR.MkExprRecordExtension e k _ v) = do
  let v' = convert v
  let (xs, e') = reduceObj e

  ((k, v') : xs, e')
reduceObj (HLIR.MkExprLoc e _) = reduceObj e
reduceObj x = ([], Just $ convert x)

operators :: [Text]
operators =
  [ "==",
    "!=",
    "<",
    "<=",
    ">",
    ">=",
    "+",
    "-",
    "*",
    "/",
    "%"
  ]

-- | Create a function bsaed on a datatype constructor, the function will return a
-- | list that starts with a special value, followed by the type name and the
-- | constructor name. The rest of the list will be the arguments of the constructor.
createFunction :: Maybe Text -> HLIR.TLIR "data" -> MLIR.MLIR "expression"
createFunction (Just typeName) (HLIR.MkDataConstructor name args) = do
  let args' = ["x" <> show i | i <- [(0 :: Int) .. length args - 1]]
  let body = MLIR.MkExprList $ [MLIR.MkExprSpecial, MLIR.MkExprLiteral (MLIR.MkLitString typeName), MLIR.MkExprLiteral (MLIR.MkLitString name)] <> map MLIR.MkExprVariable args'

  MLIR.MkExprFunction name args' body
createFunction (Just typeName) (HLIR.MkDataVariable name) = do
  let body = MLIR.MkExprList [MLIR.MkExprSpecial, MLIR.MkExprLiteral (MLIR.MkLitString typeName), MLIR.MkExprLiteral (MLIR.MkLitString name)]

  MLIR.MkExprLet name body
createFunction Nothing (HLIR.MkDataConstructor name args) = do
  let args' = ["x" <> show i | i <- [(0 :: Int) .. length args - 1]]
  let body = MLIR.MkExprList $ [MLIR.MkExprSpecial, MLIR.MkExprLiteral (MLIR.MkLitString ""), MLIR.MkExprLiteral (MLIR.MkLitString name)] <> map MLIR.MkExprVariable args'

  MLIR.MkExprFunction name args' body
createFunction Nothing (HLIR.MkDataVariable name) = do
  let body = MLIR.MkExprList [MLIR.MkExprSpecial, MLIR.MkExprLiteral (MLIR.MkLitString ""), MLIR.MkExprLiteral (MLIR.MkLitString name)]

  MLIR.MkExprLet name body

createFunction' :: Maybe Text -> HLIR.DataConstructor t -> (Text, MLIR.Expression)
createFunction' t (HLIR.MkDataConstructor name args) = do
  let args' = ["x" <> show i | i <- [(0 :: Int) .. length args - 1]]
  let body = MLIR.MkExprList $ [MLIR.MkExprSpecial, MLIR.MkExprLiteral (MLIR.MkLitString (fromMaybe "" t)), MLIR.MkExprLiteral (MLIR.MkLitString name)] <> map MLIR.MkExprVariable args'

  (name, MLIR.MkExprLambda args' body)
createFunction' t (HLIR.MkDataVariable name) = do
  let body = MLIR.MkExprList [MLIR.MkExprSpecial, MLIR.MkExprLiteral (MLIR.MkLitString (fromMaybe "" t)), MLIR.MkExprLiteral (MLIR.MkLitString name)]

  (name, body)

-- | Create a sequence of if-else expressions based on a list that contains:
-- | - A list of conditions
-- | - A map of variables that need to be substituted
-- | - The expression that needs to be evaluated
createIfs ::
  [(([MLIR.MLIR "expression"], Map Text (MLIR.MLIR "expression")), MLIR.MLIR "expression")] ->
  MLIR.MLIR "expression"
createIfs = buildDecisionTree

-- | Create a sequence of conditions based on a list of patterns
createFinalCondition :: [MLIR.MLIR "expression"] -> MLIR.MLIR "expression"
createFinalCondition [] = MLIR.MkExprLiteral (MLIR.MkLitInt 1)
createFinalCondition [x] = x
createFinalCondition (x : xs) = MLIR.MkExprTernary x (createFinalCondition xs) (MLIR.MkExprLiteral (HLIR.MkLitInt 0))

-- | Create a sequence of let expressions based on a list of variables and expressions
createLets ::
  [(Text, MLIR.MLIR "expression")] ->
  MLIR.MLIR "expression" ->
  MLIR.MLIR "expression"
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
eraseTypes (HLIR.MkExprLet _ (Left ann) e b : xs) | Just constrs <- getData e = do
  let funs = map (createFunction' (Just ann.name)) constrs
  let e' = MLIR.MkExprLet ann.name (MLIR.MkExprRecord (Map.fromList funs))
  e' : eraseTypes (b : xs)
eraseTypes (HLIR.MkExprPublic e : xs) =
  eraseTypes (e : xs)
eraseTypes (HLIR.MkExprLoc e p : xs) =
  (MLIR.MkExprLoc p <$> eraseTypes [e]) <> eraseTypes xs
eraseTypes (x : xs) =
  convert x : eraseTypes xs
eraseTypes [] = []

getData :: HLIR.TLIR "expression" -> Maybe [HLIR.DataConstructor HLIR.Type]
getData (HLIR.MkExprData cs) = Just cs
getData (HLIR.MkExprLoc e _) = getData e
getData _ = Nothing

idx :: MLIR.MLIR "expression" -> Integer -> MLIR.MLIR "expression"
idx e i = MLIR.MkExprIndex e (MLIR.MkExprLiteral (HLIR.MkLitInt i))

equalsTo :: MLIR.MLIR "expression" -> MLIR.MLIR "expression" -> MLIR.MLIR "expression"
equalsTo = MLIR.MkExprBinary "=="

sliceFrom :: MLIR.MLIR "expression" -> Integer -> MLIR.MLIR "expression"
sliceFrom e i = MLIR.MkExprApplication (MLIR.MkExprVariable "sliceFrom") [e, MLIR.MkExprLiteral (HLIR.MkLitInt i), MLIR.MkExprRecord mempty]

isVariable :: HLIR.TLIR "expression" -> Bool
isVariable (HLIR.MkExprVariable _) = True
isVariable (HLIR.MkExprLoc e _) = isVariable e
isVariable _ = False

getVariable :: HLIR.TLIR "expression" -> Text
getVariable (HLIR.MkExprVariable x) = x.name
getVariable (HLIR.MkExprLoc e _) = getVariable e
getVariable _ = compilerError "expected variable"

-- | Build a decision tree from multiple condition lists with associated expressions
buildDecisionTree ::
  [(([MLIR.MLIR "expression"], Map Text (MLIR.MLIR "expression")), MLIR.MLIR "expression")] ->
  MLIR.MLIR "expression"
buildDecisionTree [] = MLIR.MkExprApplication (MLIR.MkExprVariable "panic") [MLIR.MkExprLiteral (HLIR.MkLitString "non-exhaustive pattern")]
buildDecisionTree [((conds, maps), expr)] =
  let condition = createFinalCondition conds
      lets = createLets (Map.toList maps) expr
  in MLIR.MkExprTernary condition lets (MLIR.MkExprApplication (MLIR.MkExprVariable "panic") [MLIR.MkExprLiteral (HLIR.MkLitString "non-exhaustive pattern")])
buildDecisionTree cases =
  case findCommonConditionPrefix cases of
    ([], _) ->
      -- No common prefix, create sequence of if-else
      createSequentialIfs cases
    (commonPrefix, remainingCases) ->
      -- Common prefix found, create optimized decision tree
      let prefixCondition = createFinalCondition commonPrefix
          suffixTree = buildDecisionTree (filter (not . null . fst . fst) remainingCases)
      in MLIR.MkExprTernary prefixCondition suffixTree (MLIR.MkExprApplication (MLIR.MkExprVariable "panic") [MLIR.MkExprLiteral (HLIR.MkLitString "non-exhaustive pattern")])

-- | Create a sequential if-else chain
createSequentialIfs ::
  [(([MLIR.MLIR "expression"], Map Text (MLIR.MLIR "expression")), MLIR.MLIR "expression")] ->
  MLIR.MLIR "expression"
createSequentialIfs [] = MLIR.MkExprApplication (MLIR.MkExprVariable "panic") [MLIR.MkExprLiteral (HLIR.MkLitString "non-exhaustive pattern")]
createSequentialIfs [((conds, maps), expr)] =
  let condition = createFinalCondition conds
      lets = createLets (Map.toList maps) expr
  in MLIR.MkExprTernary condition lets (MLIR.MkExprApplication (MLIR.MkExprVariable "panic") [MLIR.MkExprLiteral (HLIR.MkLitString "non-exhaustive pattern")])
createSequentialIfs (((conds, maps), expr) : rest) =
  let condition = createFinalCondition conds
      lets = createLets (Map.toList maps) expr
      elseCase = createSequentialIfs rest
  in MLIR.MkExprTernary condition lets elseCase

-- | Find the longest common condition prefix among all cases
findCommonConditionPrefix ::
  [(([MLIR.MLIR "expression"], Map Text (MLIR.MLIR "expression")), MLIR.MLIR "expression")] ->
  ([MLIR.MLIR "expression"], [(([MLIR.MLIR "expression"], Map Text (MLIR.MLIR "expression")), MLIR.MLIR "expression")])
findCommonConditionPrefix [] = ([], [])
findCommonConditionPrefix [x] = (fst (fst x), [])
findCommonConditionPrefix cases =
  let conditionLists = map (fst . fst) cases
      commonPrefix = takeWhileCommon conditionLists
      remainingCases = map (\((conds, maps), expr)  ->
                                 ((drop (length commonPrefix) conds, maps), expr)) cases
  in (commonPrefix, remainingCases)

-- | Take elements while they are common across all lists
takeWhileCommon :: [[MLIR.MLIR "expression"]] -> [MLIR.MLIR "expression"]
takeWhileCommon [] = []
takeWhileCommon [x] = x
takeWhileCommon lists =
  let heads = map safeHead lists
  in case heads of
    [] -> []
    (Nothing : _) -> []
    (Just h : _) ->
      if all (== Just h) heads
        then h : takeWhileCommon (map safeTail lists)
        else []
  where
    safeHead [] = Nothing
    safeHead (x : _) = Just x

    safeTail [] = []
    safeTail (_ : xs) = xs

data CMap
  = ConditionMap (MLIR.MLIR "expression") CMap CMap
  | Null

conditionsToMap :: [MLIR.MLIR "expression"] -> CMap
conditionsToMap [] = Null
conditionsToMap (x : xs) = ConditionMap x (conditionsToMap xs) Null

combineConditions :: CMap -> CMap -> MLIR.Expression
combineConditions Null Null = MLIR.MkExprLiteral (HLIR.MkLitInt 1)
combineConditions (ConditionMap c1 m1 m1') (ConditionMap c2 m2 m2') =
  if c1 == c2
    then MLIR.MkExprTernary c1 (combineConditions m1 m2) (combineConditions m1' m2')
    else MLIR.MkExprTernary c1 (combineConditions m1 m2) (combineConditions m1' (ConditionMap c2 m2 m2'))
combineConditions Null (ConditionMap c2 m2 m2') =
  MLIR.MkExprTernary c2 (combineConditions Null m2) (combineConditions Null m2')
combineConditions (ConditionMap c1 m1 m1') Null =
  MLIR.MkExprTernary c1 (combineConditions m1 Null) (combineConditions m1' Null)

-- | Create a sequence of conditions based on a pattern match
-- | The function returns a list of conditions and a map of variables that need
-- | to be substituted.
createCondition ::
  MLIR.MLIR "expression" ->
  HLIR.TLIR "pattern" ->
  ([MLIR.MLIR "expression"], Map Text (MLIR.MLIR "expression"))
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
  ([MLIR.MkExprApplication (MLIR.MkExprVariable "||") [createFinalCondition conds, createFinalCondition conds']], maps <> maps')
createCondition x (HLIR.MkPatCondition e y) = do
  let (conds, maps) = createCondition x y
  let e' = convert e
  ([MLIR.MkExprApplication (MLIR.MkExprVariable "&&") [createFinalCondition conds, susbstituteMap (Map.toList maps) e']], maps)
createCondition x (HLIR.MkPatList pats slice _) =
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
          MLIR.MkExprApplication
            (MLIR.MkExprVariable ">")
            [ MLIR.MkExprApplication (MLIR.MkExprVariable "length") [x, MLIR.MkExprRecord mempty],
              MLIR.MkExprLiteral (HLIR.MkLitInt $ patLen - 1),
              MLIR.MkExprRecord mempty
            ]
        Nothing ->
          equalsTo
            (MLIR.MkExprApplication (MLIR.MkExprVariable "length") [x, MLIR.MkExprRecord mempty])
            (MLIR.MkExprLiteral (HLIR.MkLitInt (toInteger patLen)))
   in (lenCond : concat conds <> conds', mconcat maps <> maps')
createCondition x (HLIR.MkPatRecord fields) =
  let (conds, maps) = unzip $ map (\(key, pat) -> createCondition (MLIR.MkExprRecordAccess x key) pat) (Map.toList fields)
   in (concat conds, mconcat maps)

susbstituteMap :: [(Text, MLIR.MLIR "expression")] -> MLIR.MLIR "expression" -> MLIR.MLIR "expression"
susbstituteMap xs e = List.foldl (flip substitute) e xs
