module Prover.Classic (cprove, prove) where

import Formula
import State

-- | Prove a classical theorem
cprove :: Formula -> Bool
cprove = prove . singleton . simply

-- | Check provability
prove :: FormulaSet -> Bool
prove x = let (f, y) = view x in case f of
  -- Contradictory leaf
  (L0, T, Bot)            -> True
  (L0, F, Top)            -> True
  -- Replacement rules
  (L0, T, Top)            -> prove y
  (L0, F, Bot)            -> prove y
  (L1, T, Var p)          -> prove (subst True p Top y)
  (L1, F, Var p)          -> prove (subst True p Bot y)
  (L1, T, (Var p) :> Bot) -> prove (subst True p Bot y)
  (L1, F, (Var p) :> Bot) -> prove (subst True p Top y)
  -- Unary consequence rules
  (L2, T, a :& b)         -> prove (a `addT` b `addT` y)
  (L2, F, a :| b)         -> prove (a `addF` b `addF` y)
  (L2, F, a :> b)         -> prove (a `addT` b `addF` y)
  -- Binary consequence rules
  (L3, F, a :& b)         -> prove (a `addF` y) && prove (b `addF` y)
  (L3, T, a :| b)         -> prove (a `addT` y) && prove (b `addT` y)
  (L3, T, a :> b)         -> prove (a `addF` y) && prove (b `addT` y)
  -- Update priority
  (i, _, _) | i < LOCK    -> prove (f `next` y)
  -- Search exhausted
  _                       -> False
