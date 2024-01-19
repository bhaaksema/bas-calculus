module Bounding (Axiom, var, for, set) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Formula

-- | Axiom is kind of a formula
type Axiom = Formula

-- | Substitution
type Subst = M.Map String Formula

apply :: Subst -> Formula -> Formula
apply s (a :& b) = apply s a :& apply s b
apply s (a :| b) = apply s a :| apply s b
apply s (a :> b) = apply s a :> apply s b
apply s (V str)  = s M.! str
apply _ a        = a

subst :: Axiom -> S.Set Formula -> [Subst]
subst a fs = map M.fromList $ sequence [[(v, f1) | f1 <- S.toList fs] | v <- S.toList (vars a)]

instances :: (t -> S.Set Formula) -> [Axiom] -> t -> S.Set Formula
instances f axi e = S.fromList $ concat [map (`apply` a) (subst a (f e)) | a <- axi]

var :: [Axiom] -> Formula -> S.Set Formula
var = instances (S.map V . vars)

for :: [Axiom] -> Formula -> S.Set Formula
for = instances fors

set :: [Axiom] -> Formula -> S.Set Formula
set = instances cons
