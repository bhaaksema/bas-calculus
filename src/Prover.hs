module Prover (sprove, iprove, cprove) where

import Embed   (Axiom, embed)
import Formula (Formula (..), simply)

import Sequent

-- | Ruleset is based on the logic
data Logic = Int | Cl deriving Eq

-- | Prove a superintuitionistic theorem
sprove :: [Axiom] -> Formula -> Bool
sprove ax = iprove . embed ax

-- | Prove a intuitionistic theorem (m-G4ip)
iprove :: Formula -> Bool
iprove = prove Int . singleton . F . simply

-- | Prove a classical theorem (G3cp)
cprove :: Formula -> Bool
cprove = prove Cl . singleton . F . simply

-- | Check provability depending on the logic
prove :: Logic -> Sequent -> Bool
prove l y
  -- Resolve initial priority formulae
  | Just ((P0, e), x) <- view y = case e of
  T Bot -> True; F Top -> True
  -- Unary premise rules
  T Top -> prove l x; F Bot -> prove l x
  T (a :& b) -> prove l (T a +> T b +> x)
  F (a :| b) -> prove l (F a +> F b +> x)
  T ((c :& d) :> b) -> prove l (T (c :> d :> b) +> x)
  T ((c :| d) :> b) -> prove l (T (c :> b) +> T (d :> b) +> x)
  -- Priority schedule other formulae
  T (Var _) -> prove l (x <+ (P1, e))
  F (Var _) -> prove l (x <+ (P1, e))
  T (Var _ :> Bot) -> prove l (x <+ (P1, e))
  F (_ :> _) -> prove l (x <+ (P2, e))
  F (_ :& _) -> prove l (x <+ (P3, e))
  T (_ :| _) -> prove l (x <+ (P3, e))
  T (_ :> _) -> prove l (x <+ (P4, e))
  -- Resolve priority scheduled formulae
  | Just ((_, e), x) <- view y = case e of
  -- Replacement rules
  (T (Var p)) -> prove l (mapSubsti True p Top x)
  (F (Var p)) -> prove l (mapSubsti False p Bot x <+ (P5, e))
  (T (Var p :> Bot)) -> prove l (mapSubsti True p Bot x)
  -- Right implication
  (F (a :> b))
    | l == Cl || nullFs x -> prove l (T a +> F b +> x)
    | prove l (T a +> F b +> delFs x) -> True
  -- Right conjunction
  (F (a :& b)) -> all (prove l) [F a +> x, F b +> x]
  -- Left disjunction
  (T (a :| b)) -> all (prove l) [T a +> x, T b +> F a +> x]
  -- Left implication
  (T (a :> b))
    | l == Cl || nullFs x -> all (prove Cl) [T b +> x, F a +> F b +> x]
    | (c :> d) <- a -> all (any (prove l)) [[T b +> x],
      [T c +> T (d :> b) +> F d +> delFs x, F a +> F b +> x <+ (P5, e)]]
  -- Backtracking
  a -> prove l (x <+ (P5, a))
  -- Search exhausted
  | otherwise = False
