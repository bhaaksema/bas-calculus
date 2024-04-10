module Prover.Intuition (iprove, prove) where

import           Formula
import qualified Prover.Classic as C
import           State

-- | Prove a intuitionistic theorem
iprove :: Formula -> Bool
iprove = (\f -> prove f (singleton f)) . simply

-- | Check provability
prove :: Formula -> Sequent -> Bool
prove p x = let (s, (i, f), y) = view x in case (i, s, f) of
  -- Initial sequents
  (L0, L, Bot) -> True; (L0, R, Top) -> True
  -- Replacement rules
  (L0, L, Top) -> prove p y
  (L0, R, Bot) -> if nullR y
    then C.prove (unlock y) else prove p y
  (L1, L, Var q) -> prove p (subst True q Top y)
  (L1, R, Var q) -> prove p (lock s (i, f) $ subst False q Bot y)
  (L1, L, (Var q) :> Bot) -> prove p (subst True q Bot y)
  -- Unary premise rules
  (L2, L, a :& b) -> prove p (a `addL` b `addL` y)
  (L2, R, a :| b) -> prove p (a `addR` b `addR` y)
  (L2, L, (a :& b) :> c) -> prove p (a :> b :> c `addL` y)
  (L2, L, (a :| b) :> c) -> let q = fresh p in
    prove q (a :> q `addL` b :> q `addL` q :> c `addL` y)
  -- Binary premise rules
  (L3, R, a :& b) -> all (prove p) [a `addR` y, b `addR` y]
  (L3, L, a :| b) -> all (prove p) [a `addL` y, b `addL` y]
  (L3, R, a :> b) -> any (prove p) [a `addL` b `setR` y, lock s (i, f) y]
  -- Ternary premise rules
  (L4, L, (a :> b) :> c) -> let q = fresh p in
    if prove q (a `addL` b :> q `addL` q :> c `addL` q `setR` unlock y)
    then all (\pr -> pr (c `addL` unlock y)) [C.prove, prove p]
    else prove p (lock s (i, f) y)
  -- Update priority
  _ | i < LOCK -> prove p (next s (i, f) y)
  -- Search exhausted
  _ -> False
