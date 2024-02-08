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

-- | Schedule formulae based on proof rules
schedule :: Sign Formula -> Prio
schedule a = case a of
  T (Var _)        -> P1
  F (Var _)        -> P1
  T (Var _ :> Bot) -> P1
  F (_ :> _)       -> P2
  F (_ :& _)       -> P3
  T (_ :| _)       -> P3
  T (_ :> _)       -> P4
  _                -> P0

-- | Check provability depending on the logic
prove :: Logic -> Sequent -> Bool
prove l y = case view y of
  -- Search exhausted
  Empty -> False
  -- Schedule fresh (+>) formulae
  Fresh (a, x) -> prove l ((schedule a, a) |> x)
  -- Resolve scheduled (|>) formulae
  Stale (e, x) -> case e of
    -- Initial sequents
    T Bot -> True; F Top -> True
    -- Unary premise rules
    T Top -> prove l x; F Bot -> prove l x
    T (a :& b) -> prove l (T a +> T b +> x)
    F (a :| b) -> prove l (F a +> F b +> x)
    T ((c :& d) :> b) -> prove l (T (c :> d :> b) +> x)
    T ((c :| d) :> b) -> prove l (T (c :> b) +> T (d :> b) +> x)
    -- Replacement rules
    (T (Var p)) -> prove l (mapSubsti True p Top x)
    (F (Var p)) -> prove l ((P5, e) |> mapSubsti False p Bot x)
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
        [T c +> T (d :> b) +> F d +> delFs x, F a +> F b +> (P5, e) |> x]]
    -- Backtracking
    a -> prove l ((P5, a) |> x)
