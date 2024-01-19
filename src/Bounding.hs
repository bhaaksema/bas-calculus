module Bounding (Axiom, var, for, set) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Formula

-- | Axiom is kind of a formula
type Axiom = Formula

-- | Substitution from variables to formulas
type Subst = M.Map String Formula

-- | Substitute all variables in a formula
apply :: Subst -> Formula -> Formula
apply s (a :& b) = apply s a :& apply s b
apply s (a :| b) = apply s a :| apply s b
apply s (a :> b) = apply s a :> apply s b
apply s (V str)  = s M.! str
apply _ a        = a

-- | All possible substitutions of an axiom
subst :: Axiom -> S.Set Formula -> [Subst]
subst a fs = map M.fromList $ sequence [[(v, f1) | f1 <- S.toList fs] | v <- S.toList (vars a)]

-- | All possible instantiations of an axiom
insts :: (t -> S.Set Formula) -> [Axiom] -> t -> S.Set Formula
insts f axi e = S.fromList $ concat [map (`apply` a) (subst a (f e)) | a <- axi]

-- | Variable-bounding function
var :: [Axiom] -> Formula -> S.Set Formula
var = insts (S.map V . vars)

-- | Formula-bounding function
for :: [Axiom] -> Formula -> S.Set Formula
for = insts fors

-- | Set-bounding function
set :: [Axiom] -> Formula -> S.Set Formula
set = insts cons
