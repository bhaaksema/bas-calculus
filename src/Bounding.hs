module Bounding (var, for, set) where

import Data.List  (subsequences, union)
import Data.Maybe (fromJust)
import Formula    (Formula (..))

-- | Axiom is kind of a formula
type Axiom = Formula

-- | Get all unique variables of a formula
vars :: Formula -> [Formula]
vars (a :& b) = vars a `union` vars b
vars (a :| b) = vars a `union` vars b
vars (a :> b) = vars a `union` vars b
vars a        = [a | not (a == T || a == F)]

-- | Get all unique subformulas of a formula
formulas :: Formula -> [Formula]
formulas (a :& b) = formulas a `union` formulas b ++ [a :& b]
formulas (a :| b) = formulas a `union` formulas b ++ [a :| b]
formulas (a :> b) = formulas a `union` formulas b ++ [a :> b]
formulas a        = [a]

-- | Get all fusions of subformulas of a formula
fusions :: Formula -> [Formula]
fusions = map fuse . tail . subsequences . formulas
  where fuse = foldl1 (:&)

-- | Substitution
type Subst = [(Formula, Formula)]

apply :: Subst -> Formula -> Formula
apply s (a :& b) = apply s a :& apply s b
apply s (a :| b) = apply s a :| apply s b
apply s (a :> b) = apply s a :> apply s b
apply s a        = if a == T || a == F then a else fromJust $ lookup a s

subst :: Axiom -> [Formula] -> [Subst]
subst a fs = sequence [[(v, f1) | f1 <- fs] | v <- vs] where
  vs = vars a

instances :: (t -> [Formula]) -> [Formula] -> t -> [Formula]
instances f axi e = concat [map (`apply` a) (subst a (f e)) | a <- axi]

var :: [Axiom] -> Formula -> [Formula]
var = instances vars

for :: [Axiom] -> Formula -> [Formula]
for = instances formulas

set :: [Axiom] -> Formula -> [Formula]
set = instances fusions
