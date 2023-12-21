module Bounding (set) where

import Data.List  (subsequences, union)
import Data.Maybe (fromJust)
import Formula    (Formula (..))

-- | Axiom is kind of a formula
type Axiom = Formula

-- | Get all unique variables of a formula
vars :: Formula -> [String]
vars (Var v)  = [v]
vars (a :& b) = vars a `union` vars b
vars (a :| b) = vars a `union` vars b
vars (a :> b) = vars a `union` vars b
vars _        = []

-- | Get all unique subformulas of a formula
formulas :: Formula -> [Formula]
formulas (a :& b) = a :& b : formulas a `union` formulas b
formulas (a :| b) = a :| b : formulas a `union` formulas b
formulas (a :> b) = a :> b : formulas a `union` formulas b
formulas a        = [a]

-- | Get all unique fusions of subformulas of a formula
fusions :: Formula -> [Formula]
fusions = map fuse . tail . subsequences . formulas
  where fuse = foldl1 (:&)

-- | Substitution
type Subst = [(String, Formula)]

apply :: Subst -> Formula -> Formula
apply s (Var v)  = fromJust $ lookup v s
apply s (a :& b) = apply s a :& apply s b
apply s (a :| b) = apply s a :| apply s b
apply s (a :> b) = apply s a :> apply s b
apply _ a        = a

subst :: Axiom -> Formula -> [Subst]
subst a f = sequence [[(v, f1) | f1 <- fs] | v <- vs] where
  fs = fusions f
  vs = vars a

set :: [Axiom] -> Formula -> [Formula]
set axi f = concat [map (`apply` a) (subst a f) | a <- axi]
