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
prove l y | Just (e, x) <- view y = case e of
  -- Initial sequents
  (P0, T Bot) -> True; (P0, F Top) -> True
  -- Unary premise rules
  (P0, T (a :& b)) -> prove l (T a +> T b +> x)
  (P0, F (a :| b)) -> prove l (F a +> F b +> x)
  (P0, T ((c :& d) :> b)) -> prove l (T (c :> d :> b) +> x)
  (P0, T ((c :| d) :> b)) -> prove l (T (c :> b) +> T (d :> b) +> x)
  -- Replacement rules
  (P0, T Top) -> prove l x; (P0, F Bot) -> prove l x
  (P1, T (Var p)) -> prove l (mapSubsti True p Top x)
  (P1, F (Var p)) -> prove l (mapSubsti False p Bot x <+ e)
  (P1, T (Var p :> Bot)) -> prove l (mapSubsti True p Bot x)
  -- Right implication
  (P2, F (a :> b)) -> any (prove l) $ if l == Cl || nullFs x
    then [T a +> F b +> x] else [T a +> replaceFs b x, x <+ e]
  -- Right conjunction
  (P3, F (a :& b)) -> all (prove l) [F a +> x, F b +> x]
  -- Left disjunction
  (P3, T (a :| b)) -> all (prove l) [T a +> x, T b +> F a +> x]
  -- Left implication
  (P4, T (a@(c :> d) :> b)) -> prove l (T b +> x) &&
    if l == Cl || nullFs x then prove Cl (F a +> F b +> x)
    else prove l (T c +> T (d :> b) +> replaceFs d x)
      || prove l (F a +> F b +> x <+ e)
  -- Update priority
  _ -> prove l (x <+ e)
  -- Search exhausted
  | otherwise = False
