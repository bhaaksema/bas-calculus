module Prover.Intuition (iprove, prove) where

import           Formula
import qualified Prover.Classic as C
import           State

-- | Prove a intuitionistic theorem
iprove :: Formula -> Bool
iprove = (\f -> prove f (singleton f)) . simply

-- | Check provability
prove :: Formula -> FormulaSet -> Bool
prove p x = let (f, y) = view x in case f of
  -- Initial sequents
  (L0, T, Bot) -> True; (L0, F, Top) -> True
  -- Replacement rules
  (L0, T, Top) -> prove p y
  (L0, F, Bot) -> if nullFs y
    then C.prove (unlock y) else prove p y
  (L1, T, Var q) -> prove p (subst True q Top y)
  (L1, F, Var q) -> prove p (f `lock` subst False q Bot y)
  (L1, T, (Var q) :> Bot) -> prove p (subst True q Bot y)
  -- Unary premise rules
  (L2, T, a :& b) -> prove p (a `addT` b `addT` y)
  (L2, F, a :| b) -> prove p (a `addF` b `addF` y)
  (L2, T, (a :& b) :> c) -> prove p (a :> b :> c `addT` y)
  (L2, T, (a :| b) :> c) -> let q = fresh p in
    prove q (a :> q `addT` b :> q `addT` q :> c `addT` y)
  -- Binary premise rules
  (L3, F, a :& b) -> all (prove p) [a `addF` y, b `addF` y]
  (L3, T, a :| b) -> all (prove p) [a `addT` y, b `addT` y]
  (L3, F, a :> b) -> any (prove p) [a `addT` b `setF` y, f `lock` y]
  -- Ternary premise rules
  (L4, T, (a :> b) :> c) -> let q = fresh p in
    if prove q (a `addT` b :> q `addT` q :> c `addT` q `setF` unlock y)
    then all (\pr -> pr (c `addT` unlock y)) [C.prove, prove p]
    else prove p (f `lock` y)
  -- Update priority
  (i, _, _) | i < LOCK -> prove p (f `next` y)
  -- Search exhausted
  _ -> False
