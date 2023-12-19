module Prover (prove) where

import Formula (Formula (..))
import Utils   (holes)

-- | Checks axioms and then other rules
axiom :: [Formula] -> Formula -> Bool
axiom facts goal = goal `elem` facts || right facts goal

-- | Checks invertible right rules and then other rules
right :: [Formula] -> Formula -> Bool
right _ T            = True
right facts (a :& b) = axiom facts a && axiom facts b
right facts (a :> b) = axiom (a : facts) b
right facts goal     = left facts [] goal

-- | Checks invertible left rules and then other rules
left :: [Formula] -> [Formula] -> Formula -> Bool
left (f:fs) alt goal = case f of
  v@(Var _) -> left fs (v : alt) goal
  F -> True
  T -> axiom (fs ++ alt) goal
  (a :& b) -> axiom (a : b : fs ++ alt) goal
  (a :| b) -> axiom (a : fs ++ alt) goal && axiom (b : fs ++ alt) goal
  (a :> b) -> case a of
    Var _ -> if a `elem` (fs ++ alt) then axiom (b : fs ++ alt) goal
      else left fs (a :> b : alt) goal -- Store fact for later
    F -> axiom (fs ++ alt) goal
    T -> axiom (b : fs ++ alt) goal
    c :& d -> axiom ((c :> (d :> b)) : fs ++ alt) goal
    c :| d -> axiom ((c :> b) : (d :> b) : fs ++ alt) goal
    _ -> left fs (a :> b : alt) goal -- Store fact for later
left [] alt goal = nonInv alt goal

-- | Checks non-invertible rules
nonInv :: [Formula] -> Formula -> Bool
nonInv facts goal = rightOr goal || any (uncurry leftImp) (holes facts) where
  rightOr (a :| b) = axiom facts a || axiom facts b
  rightOr _        = False
  leftImp ((c :> d) :> b) fs = axiom (d :> b : fs) (c :> d) && axiom (b : fs) goal
  leftImp _ _                = False -- Skip other facts

-- | Checks if a formula is provable
prove :: Formula -> Bool
prove = axiom []
