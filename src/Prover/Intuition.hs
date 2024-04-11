module Prover.Intuition (iprove, prove) where

import           Formula
import qualified Prover.Classic as C
import           Sequent

-- | Prove a intuitionistic theorem
iprove :: Formula -> Bool
iprove = (\f -> prove f (singleton f)) . simply

-- | Check provability
prove :: Formula -> Sequent -> Bool
prove p s1 = let (h, s) = view s1 in case h of
  -- Initial sequents
  (L0, L, Bot) -> True; (L0, R, Top) -> True
  -- Replacement rules
  (L0, L, Top) -> prove p s
  (L0, R, Bot) -> if nullR s
    then C.prove (unlock s) else prove p s
  (L1, L, Var q) -> prove p (subst True q Top s)
  (L1, L, (Var q) :> Bot) -> prove p (subst True q Bot s)
  (L1, R, Var q) -> prove p (lock h $ subst False q Bot s)
  -- Unary premise rules
  (L2, L, a :& b) -> prove p (addL a $ addL b s)
  (L2, L, (a :& b) :> c) -> prove p (addL (a :> b :> c) s)
  (L2, L, (a :| b) :> c) -> let q = fresh p in
    prove q (addL (a :> q) $ addL (b :> q) $ addL (q :> c) s)
  (L2, R, a :| b) -> prove p (addR a $ addR b s)
  -- Binary premise rules
  (L3, L, a :| b) -> all (prove p) [addL a s, addL b s]
  (L3, R, a :& b) -> all (prove p) [addR a s, addR b s]
  (L3, R, a :> b) -> any (prove p) [addL a $ setR b s, lock h s]
  -- Ternary premise rules
  (L4, L, (a :> b) :> c) -> let q = fresh p in
    if prove q (addL a $ addL (b :> q) $ addL (q :> c) $ setR q $ unlock s)
    then all (\pr -> pr (addL c $ unlock s)) [C.prove, prove p]
    else prove p (lock h s)
  -- Search exhausted
  (LOCK, _, _) -> False
  -- Continue search
  _ -> prove p (next h s)
