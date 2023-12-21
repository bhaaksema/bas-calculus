module Prover (prove) where

import Bounding (set)
import Formula  (Formula (..))
import Utils    (holes)

-- | Checks axioms and then other rules
axiom :: [Formula] -> [Formula] -> Formula -> Bool
axiom axi facts goal = goal `elem` facts || right axi facts goal

-- | Checks invertible right rules and then other rules
right :: [Formula] -> [Formula] -> Formula -> Bool
right _ _ T              = True
right axi facts (a :& b) = axiom axi facts a && axiom axi facts b
right axi facts ((c :& d) :| b) = right axi facts ((c :| b) :& (d :| b))
right axi facts (a :| (c :& d)) = right axi facts ((a :| c) :& (a :| d))
right axi facts (a :| b)
  | a == T || b == T     = True
  | a == b               = axiom axi facts a
  | b < a                = right axi facts (b :| a)
  | otherwise            = left axi facts [] (a :| b)
right axi facts (a :> b) = axiom axi (a : facts) b
right axi facts goal     = left axi facts [] goal

-- | Checks invertible left rules and then other rules
left :: [Formula] -> [Formula] -> [Formula] -> Formula -> Bool
left axi (f:fs) alt goal = case f of
  v@(Var _) -> left axi fs (v : alt) goal
  F -> True
  T -> axiom axi (fs ++ alt) goal
  (a :& b) -> axiom axi (a : b : fs ++ alt) goal
  (a :| b) -> axiom axi (a : fs ++ alt) goal && axiom axi (b : fs ++ alt) goal
  (a :> b) -> case a of
    Var _ -> if a `elem` (fs ++ alt) then axiom axi (b : fs ++ alt) goal
      else left axi fs (a :> b : alt) goal -- Store fact for later
    F -> axiom axi (fs ++ alt) goal
    T -> axiom axi (b : fs ++ alt) goal
    c :& d -> axiom axi ((c :> (d :> b)) : fs ++ alt) goal
    c :| d -> axiom axi ((c :> b) : (d :> b) : fs ++ alt) goal
    _ -> left axi fs (a :> b : alt) goal -- Store fact for later
left axi [] alt goal = nonInv axi alt goal

-- | Checks non-invertible rules
nonInv :: [Formula] -> [Formula] -> Formula -> Bool
nonInv axi facts goal = rightOr goal || any (uncurry leftImp) (holes facts) || cut axi where
  rightOr (a :| b) = axiom axi facts a || axiom axi facts b
  rightOr _        = False
  leftImp ((c :> d) :> b) fs = axiom axi (d :> b : fs) (c :> d) && axiom axi (b : fs) goal
  leftImp _ _                = False -- Skip other facts
  cut []       = False
  cut (a : as) = axiom as (a : facts) goal

-- | Checks if a formula is provable
prove :: [Formula] -> Formula -> Bool
prove as f = axiom (set as f) [] f
