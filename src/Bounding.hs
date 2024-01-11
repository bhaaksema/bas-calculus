module Bounding (Axiom, var, for, set) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Formula

-- | Axiom is kind of a formula
type Axiom = Formula

-- | Get all fusions of subformulas of a formula
fusions :: Formula -> S.Set Formula
fusions = S.map fuse . S.delete S.empty . S.powerSet . fors
  where fuse = foldl1 (:&)

-- | Substitution
type Subst = M.Map String Formula

apply :: Subst -> Formula -> Formula
apply s (a :& b) = apply s a :& apply s b
apply s (a :| b) = apply s a :| apply s b
apply s (a :> b) = apply s a :> apply s b
apply s (V str)  = s M.! str
apply _ a        = a

subst :: Axiom -> S.Set Formula -> [Subst]
subst a fs = map M.fromList $ sequence [[(v, f1) | f1 <- S.toList fs] | v <- S.toList vs]
  where vs = vars a

instances :: (t -> S.Set Formula) -> [Axiom] -> t -> [Formula]
instances f axi e = concat [map (`apply` a) (subst a (f e)) | a <- axi]

var :: [Axiom] -> Formula -> [Formula]
var = instances (S.map V . vars)

for :: [Axiom] -> Formula -> [Formula]
for = instances fors

set :: [Axiom] -> Formula -> [Formula]
set = instances fusions
