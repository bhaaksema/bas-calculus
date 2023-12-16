module Bounding (set) where

import Data.List  (subsequences, union)
import Data.Maybe (fromMaybe)
import Formula    (Formula (..))

-- | Axiom is kind of a formula
type Axiom = Formula

-- | Get all unique variables of a formula
vars :: Formula -> [String]
vars (Var v)   = [v]
vars (And f g) = vars f `union` vars g
vars (Or f g)  = vars f `union` vars g
vars (Imp f g) = vars f `union` vars g
vars _         = []

-- | Get all unique subformulas of a formula
formulas :: Formula -> [Formula]
formulas (Var v)   = [Var v]
formulas (And f g) = formulas f `union` formulas g `union` [And f g]
formulas (Or f g)  = formulas f `union` formulas g `union` [Or f g]
formulas (Imp f g) = formulas f `union` formulas g `union` [Imp f g]
formulas f         = [f]

-- | Get all unique fusions of subformulas of a formula
fusions :: Formula -> [Formula]
fusions = map fuse . tail . subsequences . formulas
  where fuse = foldr1 And

-- | Substitution
type Subst = [(String, Formula)]

apply :: Subst -> Formula -> Formula
apply s (Var v)   = fromMaybe (Var v) (lookup v s)
apply s (And f g) = And (apply s f) (apply s g)
apply s (Or f g)  = Or (apply s f) (apply s g)
apply s (Imp f g) = Imp (apply s f) (apply s g)
apply _ f         = f

fusionSubst :: Axiom -> Formula -> [Subst]
fusionSubst a f = [[(v, f1) | f1 <- fusions f] | v <- vars a]

set :: [Axiom] -> Formula -> [Formula]
set as f = concat [map (`apply` a) (fusionSubst a f) | a <- as]
