module Prover.Classic (cprove, prove) where

import Formula
import State

-- | Prove a classical theorem
cprove :: Formula -> Bool
cprove = prove . singleton . simply

-- | Check provability
prove :: Sequent -> Bool
prove x = let (s, (i, f), y) = view x in case (i, s, f) of
  -- Initial sequents
  (L0, L, Bot)            -> True
  (L0, R, Top)            -> True
  -- Replacement rules
  (L0, L, Top)            -> prove y
  (L0, R, Bot)            -> prove y
  (L1, L, Var p)          -> prove (subst True p Top y)
  (L1, R, Var p)          -> prove (subst True p Bot y)
  (L1, L, (Var p) :> Bot) -> prove (subst True p Bot y)
  (L1, R, (Var p) :> Bot) -> prove (subst True p Top y)
  -- Unary premise rules
  (L2, L, a :& b)         -> prove (a `addL` b `addL` y)
  (L2, R, a :| b)         -> prove (a `addR` b `addR` y)
  (L2, R, a :> b)         -> prove (a `addL` b `addR` y)
  -- Binary premise rules
  (L3, R, a :& b)         -> prove (a `addR` y) && prove (b `addR` y)
  (L3, L, a :| b)         -> prove (a `addL` y) && prove (b `addL` y)
  (L3, L, a :> b)         -> prove (a `addR` y) && prove (b `addL` y)
  -- Update priority
  _ | i < LOCK            -> prove (next s (i, f) y)
  -- Search exhausted
  _                       -> False
