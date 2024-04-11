module Prover.Classic (cprove, prove) where

import Formula
import Sequent

-- | Prove a classical theorem
cprove :: Formula -> Bool
cprove = prove . singleton . simply

-- | Check provability
prove :: Sequent -> Bool
prove s1 = let (i, h, s) = view s1 in case i of
  LOCK -> False -- Search exhausted
  INIT -> case h of
    -- Initial sequents
    (L, Bot)    -> True
    (R, Top)    -> True
    -- Replacement rules
    (L, Top)    -> prove s
    (R, Bot)    -> prove s
    (L, Var p)  -> prove (subst True p Top s)
    (R, Var p)  -> prove (subst True p Bot s)
    -- Unary premise rules
    (L, Neg a)  -> prove (addR a s)
    (R, Neg a)  -> prove (addL a s)
    (L, a :& b) -> prove (addL a $ addL b s)
    (R, a :| b) -> prove (addR a $ addR b s)
    (R, a :> b) -> prove (addL a $ addR b s)
    -- Scheduling
    _           -> prove (push L1 h s)
  _ -> case h of
    -- Binary premise rules
    (L, a :| b) -> prove (addL a s) && prove (addL b s)
    (L, a :> b) -> prove (addR a s) && prove (addL b s)
    (R, a :& b) -> prove (addR a s) && prove (addR b s)
    _           -> undefined
