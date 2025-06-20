module Language.Bonzai.Backend.Closure.Free where

import qualified Language.Bonzai.Syntax.MLIR as MLIR
import qualified Data.Set as Set

class Free a where
  free :: a -> Set Text

instance (Free a) => Free [a] where
  free = foldMap free

instance (Free a) => Free (Maybe a) where
  free = foldMap free

instance (Free a, Free b) => Free (a, b) where
  free (a, b) = free a <> free b

instance Free MLIR.Expression where
  free (MLIR.MkExprVariable a) = Set.singleton a
  free (MLIR.MkExprApplication f args) = free f <> free args
  free (MLIR.MkExprLambda as e) = free e Set.\\ Set.fromList as
  free (MLIR.MkExprTernary c t e) = free c <> free t <> free e
  free (MLIR.MkExprUpdate u e) = free u <> free e
  free (MLIR.MkExprLet a e) = free e Set.\\ Set.singleton a
  free (MLIR.MkExprMut e) = free e
  free (MLIR.MkExprBlock es) = freeBlock es
    where
      freeBlock :: [MLIR.Expression] -> Set Text
      freeBlock [] = mempty
      freeBlock (MLIR.MkExprLet a e:es') = (free e <> freeBlock es') Set.\\ Set.singleton a
      freeBlock (MLIR.MkExprUnpack a e b : es') = (free e <> freeBlock (b:es')) Set.\\ Set.singleton a
      freeBlock (MLIR.MkExprLoc _ e:es') = freeBlock (e : es')
      freeBlock (e:es') = free e <> freeBlock es'
  free (MLIR.MkExprList es) = free es
  free (MLIR.MkExprNative n _) = Set.singleton n.name
  free (MLIR.MkExprIndex e i) = free e <> free i
  free (MLIR.MkExprLiteral _) = Set.empty
  free (MLIR.MkExprUnpack n e e') = (free e <> free e') Set.\\ Set.singleton n
  free (MLIR.MkExprLoc _ e) = free e
  free (MLIR.MkExprWhile c e) = free c <> free e
  free MLIR.MkExprSpecial = Set.empty
  free (MLIR.MkExprBinary _ e1 e2) = free e1 <> free e2
  free (MLIR.MkExprRecordAccess e _) = free e
  free (MLIR.MkExprSingleIf c e) = free c <> free e
  free (MLIR.MkExprReturn e) = free e
  free (MLIR.MkExprRecord m) = foldMap free m
  free MLIR.MkExprBreak = Set.empty
  free MLIR.MkExprContinue = Set.empty
  free (MLIR.MkExprSpawn e) = free e

instance Free MLIR.Update where
  free (MLIR.MkUpdtVariable a) = Set.singleton a
  free (MLIR.MkUpdtField u _) = free u
  free (MLIR.MkUpdtIndex u e) = free u <> free e

class Substitutable a b where
  substitute :: (Text, b) -> a -> a

instance Substitutable MLIR.Expression MLIR.Expression where
  substitute (a, e) (MLIR.MkExprVariable a') = if a == a' then e else MLIR.MkExprVariable a'
  substitute (a, e) (MLIR.MkExprApplication f args) = MLIR.MkExprApplication (substitute (a, e) f) (map (substitute (a, e)) args)
  substitute (a, e) (MLIR.MkExprLambda as e') = MLIR.MkExprLambda as $ if a `elem` as then e' else substitute (a, e) e'
  substitute (a, e) (MLIR.MkExprTernary c t e') = MLIR.MkExprTernary (substitute (a, e) c) (substitute (a, e) t) (substitute (a, e) e')
  substitute (a, e) (MLIR.MkExprUpdate u e') = MLIR.MkExprUpdate (substitute (a, e) u) (substitute (a, e) e')
  substitute (a, e) (MLIR.MkExprLet a' e') = MLIR.MkExprLet a' $ if a == a' then e' else substitute (a, e) e'
  substitute (a, e) (MLIR.MkExprMut e') = MLIR.MkExprMut $ substitute (a, e) e'
  substitute (a, e) (MLIR.MkExprBlock es) = MLIR.MkExprBlock $ map (substitute (a, e)) es
  substitute (a, e) (MLIR.MkExprList es) = MLIR.MkExprList $ map (substitute (a, e)) es
  substitute _ (MLIR.MkExprNative n ty) = MLIR.MkExprNative n ty
  substitute _ (MLIR.MkExprLiteral l) = MLIR.MkExprLiteral l
  substitute r (MLIR.MkExprIndex e i) = MLIR.MkExprIndex (substitute r e) (substitute r i)
  substitute r (MLIR.MkExprUnpack n e e') = MLIR.MkExprUnpack n (substitute r e) (substitute r e')
  substitute r (MLIR.MkExprLoc p e) = MLIR.MkExprLoc p (substitute r e)
  substitute r (MLIR.MkExprWhile c e) = MLIR.MkExprWhile (substitute r c) (substitute r e)
  substitute _ MLIR.MkExprSpecial = MLIR.MkExprSpecial
  substitute (a, e) (MLIR.MkExprBinary op e1 e2) = MLIR.MkExprBinary op (substitute (a, e) e1) (substitute (a, e) e2)
  substitute (a, e) (MLIR.MkExprRecordAccess e' f) = MLIR.MkExprRecordAccess (substitute (a, e) e') f
  substitute (a, e) (MLIR.MkExprSingleIf c e') = MLIR.MkExprSingleIf (substitute (a, e) c) (substitute (a, e) e')
  substitute (a, e) (MLIR.MkExprReturn e') = MLIR.MkExprReturn $ substitute (a, e) e'
  substitute (a, e) (MLIR.MkExprRecord m) = MLIR.MkExprRecord $ fmap (substitute (a, e)) m
  substitute _ MLIR.MkExprBreak = MLIR.MkExprBreak
  substitute _ MLIR.MkExprContinue = MLIR.MkExprContinue
  substitute (a, e) (MLIR.MkExprSpawn e') = MLIR.MkExprSpawn $ substitute (a, e) e'

instance Substitutable MLIR.Update MLIR.Update where
  substitute (a, e) (MLIR.MkUpdtVariable a') = if a == a' then e else MLIR.MkUpdtVariable a'
  substitute (a, e) (MLIR.MkUpdtField u f) = MLIR.MkUpdtField (substitute (a, e) u) f
  substitute (a, e) (MLIR.MkUpdtIndex u e') = MLIR.MkUpdtIndex (substitute (a, e) u) e'

instance Substitutable MLIR.Update MLIR.Expression where
  substitute _ (MLIR.MkUpdtVariable a') = MLIR.MkUpdtVariable a'
  substitute (a, e) (MLIR.MkUpdtField u f) = MLIR.MkUpdtField (substitute (a, e) u) f
  substitute (a, e) (MLIR.MkUpdtIndex u e') = MLIR.MkUpdtIndex (substitute (a, e) u) (substitute (a, e) e')
