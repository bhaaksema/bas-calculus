module Prover (prove0) where

import Bounding (set)
import Formula  (Formula (..))
import Utils    (holes)

-- | Checks axioms and other rules
prove :: [Formula] -> [Formula] -> Formula -> Bool
prove axi facts goal = any (\f -> f == goal || f == F) facts ||
  right axi facts goal || left axi facts [] goal || nonInv axi facts goal

-- | Checks invertible right rules
right :: [Formula] -> [Formula] -> Formula -> Bool
right axi facts (a :& b) = prove axi facts a && prove axi facts b
right axi facts (a :| b)
  | a == b               = prove axi facts a
  | b < a                = prove axi facts (b :| a)
  | otherwise            = a == T || b == T
right axi facts (a :> b) = prove axi (a : facts) b
right _ _ goal           = goal == T

-- | Checks invertible left rules
left :: [Formula] -> [Formula] -> [Formula] -> Formula -> Bool
left axi (f:fs) alt goal = case f of
  (a :& b)  -> prove axi (a : b : fs ++ alt) goal
  (a :| b)  -> prove axi (a : fs ++ alt) goal && prove axi (b : fs ++ alt) goal
  (a :> b)  -> case a of
    Var _ -> if a `elem` (fs ++ alt) then prove axi (b : fs ++ alt) goal
      else left axi fs (a :> b : alt) goal -- Store fact for later
    F -> prove axi (fs ++ alt) goal
    T -> prove axi (b : fs ++ alt) goal
    c :& d -> prove axi ((c :> (d :> b)) : fs ++ alt) goal
    c :| d -> prove axi ((c :> b) : (d :> b) : fs ++ alt) goal
    _ -> left axi fs (a :> b : alt) goal -- Store fact for later
  a -> left axi fs (if a == T then alt else a : alt) goal
left _ [] _ _ = False

-- | Checks non-invertible rules
nonInv :: [Formula] -> [Formula] -> Formula -> Bool
nonInv axi facts goal = rightOr goal || any (uncurry leftImp) (holes facts) || cut axi where
  rightOr (a :| b) = prove axi facts a || prove axi facts b
  rightOr _        = False
  leftImp ((c :> d) :> b) fs = prove axi (d :> b : fs) (c :> d) && prove axi (b : fs) goal
  leftImp _ _                = False -- Skip other facts
  cut []       = False
  cut (a : as) = prove as (a : facts) goal

-- | Checks if a formula is provable
prove0 :: [Formula] -> Formula -> Bool
prove0 as f = prove (set as f) [] f
