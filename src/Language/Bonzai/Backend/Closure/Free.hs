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
  free (MLIR.MkExprMut a e) = free e Set.\\ Set.singleton a
  free (MLIR.MkExprBlock es) = freeBlock es
    where
      freeBlock :: [MLIR.Expression] -> Set Text
      freeBlock [] = mempty
      freeBlock (MLIR.MkExprLet a e:es') = (free e <> freeBlock es') Set.\\ Set.singleton a
      freeBlock (MLIR.MkExprLoc _ e:es') = freeBlock (e : es')
      freeBlock (e:es') = free e <> freeBlock es'
  free (MLIR.MkExprEvent es) = free es
  free (MLIR.MkExprOn _ as e) = free e Set.\\ Set.fromList as
  free (MLIR.MkExprSend e _ es) = free e <> free es
  free (MLIR.MkExprSpawn e) = free e
  free (MLIR.MkExprList es) = free es
  free (MLIR.MkExprNative n _) = Set.singleton n.name
  free (MLIR.MkExprIndex e i) = free e <> free i
  free (MLIR.MkExprLiteral _) = Set.empty
  free (MLIR.MkExprUnpack n e e') = free e <> (free e' Set.\\ Set.singleton n)
  free (MLIR.MkExprLoc _ e) = free e

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
  substitute (a, e) (MLIR.MkExprMut a' e') = MLIR.MkExprMut a' $ if a == a' then e' else substitute (a, e) e'
  substitute (a, e) (MLIR.MkExprBlock es) = MLIR.MkExprBlock $ map (substitute (a, e)) es
  substitute (a, e) (MLIR.MkExprEvent es) = MLIR.MkExprEvent $ map (substitute (a, e)) es
  substitute (a, e) (MLIR.MkExprOn ev as e') = MLIR.MkExprOn ev as $ if a `elem` as then e' else substitute (a, e) e'
  substitute (a, e) (MLIR.MkExprSend e' ev es) = MLIR.MkExprSend (substitute (a, e) e') ev $ map (substitute (a, e)) es
  substitute (a, e) (MLIR.MkExprSpawn e') = MLIR.MkExprSpawn $ substitute (a, e) e'
  substitute (a, e) (MLIR.MkExprList es) = MLIR.MkExprList $ map (substitute (a, e)) es
  substitute _ (MLIR.MkExprNative n ty) = MLIR.MkExprNative n ty
  substitute _ (MLIR.MkExprLiteral l) = MLIR.MkExprLiteral l
  substitute r (MLIR.MkExprIndex e i) = MLIR.MkExprIndex (substitute r e) (substitute r i)
  substitute r (MLIR.MkExprUnpack n e e') = MLIR.MkExprUnpack n (substitute r e) (substitute r e')
  substitute r (MLIR.MkExprLoc p e) = MLIR.MkExprLoc p (substitute r e)

instance Substitutable MLIR.Update MLIR.Update where
  substitute (a, e) (MLIR.MkUpdtVariable a') = if a == a' then e else MLIR.MkUpdtVariable a'
  substitute (a, e) (MLIR.MkUpdtField u f) = MLIR.MkUpdtField (substitute (a, e) u) f
  substitute (a, e) (MLIR.MkUpdtIndex u e') = MLIR.MkUpdtIndex (substitute (a, e) u) e'

instance Substitutable MLIR.Update MLIR.Expression where
  substitute _ (MLIR.MkUpdtVariable a') = MLIR.MkUpdtVariable a'
  substitute (a, e) (MLIR.MkUpdtField u f) = MLIR.MkUpdtField (substitute (a, e) u) f
  substitute (a, e) (MLIR.MkUpdtIndex u e') = MLIR.MkUpdtIndex (substitute (a, e) u) (substitute (a, e) e')
