module Bounding (Axiom, var, for, set) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import           Formula

-- | Axiom is kind of a formula
type Axiom = Formula

-- | Get all unique variables of a formula
vars :: Formula -> Set.Set String
vars (a :& b) = vars a `Set.union` vars b
vars (a :| b) = vars a `Set.union` vars b
vars (a :> b) = vars a `Set.union` vars b
vars (Var a)  = Set.singleton a
vars _        = Set.empty

-- | Get all unique subformulas of a formula
formulas :: Formula -> Set.Set Formula
formulas (a :& b) = Set.insert (a :& b) $ formulas a `Set.union` formulas b
formulas (a :| b) = Set.insert (a :| b) $ formulas a `Set.union` formulas b
formulas (a :> b) = Set.insert (a :> b) $ formulas a `Set.union` formulas b
formulas a        = Set.singleton a

-- | Get all fusions of subformulas of a formula
fusions :: Formula -> Set.Set Formula
fusions = Set.map fuse . Set.delete Set.empty . Set.powerSet . formulas
  where fuse = foldl1 (:&)

-- | Substitution
type Subst = Map.Map String Formula

apply :: Subst -> Formula -> Formula
apply s (a :& b) = apply s a :& apply s b
apply s (a :| b) = apply s a :| apply s b
apply s (a :> b) = apply s a :> apply s b
apply s (Var a)  = s Map.! a
apply _ a        = a

subst :: Axiom -> Set.Set Formula -> [Subst]
subst a fs = map Map.fromList $ sequence [[(v, f1) | f1 <- Set.toList fs] | v <- Set.toList vs]
  where vs = vars a

instances :: (t -> Set.Set Formula) -> [Axiom] -> t -> [Formula]
instances f axi e = concat [map (`apply` a) (subst a (f e)) | a <- axi]

var :: [Axiom] -> Formula -> [Formula]
var = instances (Set.map Var . vars)

for :: [Axiom] -> Formula -> [Formula]
for = instances formulas

set :: [Axiom] -> Formula -> [Formula]
set = instances fusions
