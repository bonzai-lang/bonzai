module Language.Bonzai.Backend.LLIR.Free where
import qualified Language.Bonzai.Syntax.MLIR as MLIR
import qualified Data.Set as Set

type Reserved = Set Text

class Free a where
  free :: Reserved -> a -> Set Text

-- Used to get the name of a function, declaration or native function
class Name a where
  getNames :: a -> Set Text

instance (Free a) => Free [a] where
  free r = foldMap (free r)

instance Free MLIR.Expression where
  free res (MLIR.MkExprVariable n) = Set.singleton n Set.\\ res
  free res (MLIR.MkExprApplication f args) = free res f <> free res args
  free res (MLIR.MkExprLambda as e) = free res e <> Set.fromList as
  free res (MLIR.MkExprTernary c t e) = free res c <> free res t <> free res e
  free res (MLIR.MkExprUpdate u e) = free res u <> free res e
  free res (MLIR.MkExprLet a e) = free res e <> Set.singleton a
  free res (MLIR.MkExprMut e) = free res e 
  free res (MLIR.MkExprBlock es) = freeBlock res es
    where
      freeBlock :: Reserved -> [MLIR.Expression] -> Set Text
      freeBlock _ [] = mempty
      freeBlock r (MLIR.MkExprLet a e:es') = free r e <> freeBlock r es' <> Set.singleton a
      freeBlock r (MLIR.MkExprMut e:es') = free r e <> freeBlock r es'
      freeBlock r (MLIR.MkExprLoc _ e:es') = free r e <> freeBlock r es'
      freeBlock r (e:es') = free r e <> freeBlock r es'
  free res (MLIR.MkExprList es) = free res es
  free _ (MLIR.MkExprNative {}) = mempty
  free res (MLIR.MkExprIndex e i) = free res e <> free res i
  free _ (MLIR.MkExprLiteral _) = Set.empty
  free res (MLIR.MkExprUnpack n e e') = free res e <> free res e' <> Set.singleton n
  free res (MLIR.MkExprLoc _ e) = free res e
  free res (MLIR.MkExprWhile c e) = free res c <> free res e
  free _ MLIR.MkExprSpecial = Set.empty
  free res (MLIR.MkExprBinary _ e1 e2) = free res e1 <> free res e2
  free res (MLIR.MkExprRecordAccess e _) = free res e
  free res (MLIR.MkExprSingleIf c e) = free res c <> free res e
  free res (MLIR.MkExprReturn e) = free res e
  free res (MLIR.MkExprRecord m) = foldMap (free res) m
 
instance Free MLIR.Update where
  free res (MLIR.MkUpdtVariable a) = Set.singleton a Set.\\ res
  free res (MLIR.MkUpdtField u _) = free res u
  free res (MLIR.MkUpdtIndex u e) = free res u <> free res e

instance Name a => Name [a] where
  getNames = foldMap getNames

instance Name MLIR.Expression where
  getNames (MLIR.MkExprNative n _) = Set.singleton n.name
  getNames (MLIR.MkExprFunction {}) = mempty
  getNames (MLIR.MkExprLet n _) = Set.singleton n
  getNames (MLIR.MkExprLoc _ e) = getNames e
  getNames _ = mempty
  