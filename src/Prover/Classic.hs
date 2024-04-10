module Prover.Classic (cprove, prove) where

import Formula
import Sequent

-- | Prove a classical theorem
cprove :: Formula -> Bool
cprove = prove . singleton . simply

-- | Check provability
prove :: Sequent -> Bool
prove s1 = let (lr, (i, f), s) = view s1 in case (i, lr, f) of
  -- Initial sequents
  (L0, L, Bot)            -> True
  (L0, R, Top)            -> True
  -- Replacement rules
  (L0, L, Top)            -> prove s
  (L0, R, Bot)            -> prove s
  (L1, L, Var p)          -> prove (subst True p Top s)
  (L1, R, Var p)          -> prove (subst True p Bot s)
  (L1, L, (Var p) :> Bot) -> prove (subst True p Bot s)
  (L1, R, (Var p) :> Bot) -> prove (subst True p Top s)
  -- Unary premise rules
  (L2, L, a :& b)         -> prove (addL a $ addL b s)
  (L2, R, a :| b)         -> prove (addR a $ addR b s)
  (L2, R, a :> b)         -> prove (addL a $ addR b s)
  -- Binary premise rules
  (L3, R, a :& b)         -> prove (addR a s) && prove (addR b s)
  (L3, L, a :| b)         -> prove (addL a s) && prove (addL b s)
  (L3, L, a :> b)         -> prove (addR a s) && prove (addL b s)
  -- Update priority
  _ | i < LOCK            -> prove (next lr (i, f) s)
  -- Search exhausted
  _                       -> False
