module Language.Bonzai.Backend.TypeErasure.Conversion where

import qualified Language.Bonzai.Syntax.HLIR as HLIR
import qualified Language.Bonzai.Syntax.MLIR as MLIR
import Control.Monad.Result (compilerError)

convert :: HLIR.TLIR "expression" -> MLIR.MLIR "expression"
convert (HLIR.MkExprLiteral l) = MLIR.MkExprLiteral l
convert (HLIR.MkExprVariable a) = MLIR.MkExprVariable a.name
convert (HLIR.MkExprApplication f args) = MLIR.MkExprApplication (convert f) (map convert args)
convert (HLIR.MkExprLet ann e) = MLIR.MkExprLet ann.name (convert e)
convert (HLIR.MkExprBlock es) = MLIR.MkExprBlock (map convert es)
convert (HLIR.MkExprActor _ es) = MLIR.MkExprEvent (map convert es)
convert (HLIR.MkExprOn ev as e) = MLIR.MkExprOn ev (map (.name) as) (convert e)
convert (HLIR.MkExprSend e ev es) = MLIR.MkExprSend (convert e) ev (map convert es)
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

convertUpdate :: HLIR.TLIR "update" -> MLIR.MLIR "update"
convertUpdate (HLIR.MkUpdtVariable a) = MLIR.MkUpdtVariable a.name
convertUpdate (HLIR.MkUpdtField u f) = MLIR.MkUpdtField (convertUpdate u) f
convertUpdate (HLIR.MkUpdtIndex u e) = MLIR.MkUpdtIndex (convertUpdate u) (convert e)

eraseTypes :: [HLIR.TLIR "expression"] -> [MLIR.MLIR "expression"]
eraseTypes = map convert
