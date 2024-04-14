module Prover.Classic (cprove, prove) where

import Formula
import Sequent

-- | Prove a classical theorem
cprove :: Formula -> Bool
cprove = prove . singleton . simply

-- | Check provability
prove :: Sequent -> Bool
prove s1 = let (i, h, s) = view s1 in case i of
  C1 -> case h of
    -- Initial sequents
    (L, Bot)    -> True
    (R, Top)    -> True
    -- Replacement rules
    (L, Top)    -> prove s
    (R, Bot)    -> prove s
    (L, Var p)  -> prove (subst True p Top s)
    (R, Var p)  -> prove (subst True p Bot s)
    -- Unary premise rules
    (L, Neg a)  -> prove (add R a s)
    (R, Neg a)  -> prove (add L a s)
    (L, a :& b) -> prove (add L a $ add L b s)
    (R, a :| b) -> prove (add R a $ add R b s)
    (R, a :> b) -> prove (add L a $ add R b s)
    -- Scheduling
    _           -> prove (push C2 h s)
  C2 -> case h of
    -- Binary premise rules
    (L, a :| b) -> prove (add L a s) && prove (add L b s)
    (L, a :> b) -> prove (add R a s) && prove (add L b s)
    (R, a :& b) -> prove (add R a s) && prove (add R b s)
    _           -> undefined
  -- Search exhausted
  _ -> False
