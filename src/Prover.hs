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
left facts alt goal = case facts of
  (v@(Var _) : fs) -> left fs (v : alt) goal
  (F : _) -> True
  (T : fs) -> axiom (fs ++ alt) goal
  (a :& b : fs) -> axiom (a : b : fs ++ alt) goal
  (a :| b : fs) -> axiom (a : fs ++ alt) goal && axiom (b : fs ++ alt) goal
  (a :> b : fs) -> case a of
    Var _ -> if a `elem` (facts ++ alt)
      then axiom (b : fs ++ alt) goal
      else left fs (a :> b : alt) goal -- Store fact for later
    F -> axiom (fs ++ alt) goal
    T -> axiom (b : fs ++ alt) goal
    c :& d -> axiom ((c :> (d :> b)) : fs ++ alt) goal
    c :| d -> axiom ((c :> b) : (d :> b) : fs ++ alt) goal
    _ -> left fs (a :> b : alt) goal -- Store fact for later
  [] -> nonInv alt goal

-- | Checks non-invertible rules
nonInv :: [Formula] -> Formula -> Bool
nonInv facts goal
  = (case goal of -- Right disjunction
    (a :| b) -> axiom facts a || axiom facts b
    _        -> False
  ) || any (\(f, fs) -> case f of -- Left nested implication
    ((c :> d) :> b) -> axiom (d :> b : fs) (c :> d) && axiom (b : fs) goal
    _               -> False -- Ignore other stored formulas
  ) (holes facts)

-- | Checks if a formula is provable
prove :: Formula -> Bool
prove = axiom []
