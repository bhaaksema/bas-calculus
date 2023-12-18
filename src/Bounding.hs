module Bounding (set) where

import Data.List  (subsequences, union)
import Data.Maybe (fromMaybe)
import Formula    (Formula (..))

-- | Axiom is kind of a formula
type Axiom = Formula

-- | Get all unique variables of a formula
vars :: Formula -> [String]
vars (Var v)  = [v]
vars (f :& g) = vars f `union` vars g
vars (f :| g) = vars f `union` vars g
vars (f :> g) = vars f `union` vars g
vars _        = []

-- | Get all unique subformulas of a formula
formulas :: Formula -> [Formula]
formulas (Var v)  = [Var v]
formulas (f :& g) = formulas f `union` formulas g `union` [f :& g]
formulas (f :| g) = formulas f `union` formulas g `union` [f :| g]
formulas (f :> g) = formulas f `union` formulas g `union` [f :> g]
formulas f        = [f]

-- | Get all unique fusions of subformulas of a formula
fusions :: Formula -> [Formula]
fusions = map fuse . tail . subsequences . formulas
  where fuse = foldr1 (:&)

-- | Substitution
type Subst = [(String, Formula)]

apply :: Subst -> Formula -> Formula
apply s (Var v)  = fromMaybe (Var v) (lookup v s)
apply s (f :& g) = apply s f :& apply s g
apply s (f :| g) = apply s f :| apply s g
apply s (f :> g) = apply s f :> apply s g
apply _ f        = f

fusionSubst :: Axiom -> Formula -> [Subst]
fusionSubst a f = [[(v, f1) | f1 <- fusions f] | v <- vars a]

set :: [Axiom] -> Formula -> [Formula]
set as f = concat [map (`apply` a) (fusionSubst a f) | a <- as]
