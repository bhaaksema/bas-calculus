module Prover.Classic (cprove, prove) where

import Control.Monad.State

import Formula
import State

-- | Prove a classical theorem
cprove :: Formula -> Bool
cprove = evalState prove . newState . simply

-- | Check provability
prove :: State ProverState Bool
prove = view >>= \h -> case h of
  -- Contradictory leaf
  (L0, T, Bot) -> return True
  (L0, F, Top) -> return True
  -- Replacement rules
  (L0, T, Top) -> prove
  (L0, F, Bot) -> prove
  (L1, T, Var p) -> subst True p Top >> prove
  (L1, F, Var p) -> subst True p Bot >> prove
  (L1, T, (Var p) :> Bot) -> subst True p Bot >> prove
  (L1, F, (Var p) :> Bot) -> subst True p Top >> prove
  -- Unary consequence rules
  (L2, T, a :& b) -> addT a >> addT b >> prove
  (L2, F, a :| b) -> addF a >> addF b >> prove
  (L2, F, a :> b) -> addT a >> addF b >> prove
  -- Binary consequence rules
  (L3, F, a :& b) -> do
    x <- get
    resA <- addF a >> prove
    resB <- put x >> addF b >> prove
    return (resA && resB)
  (L3, T, a :| b) -> do
    x <- get
    resA <- addT a >> prove
    resB <- put x >> addT b >> prove
    return (resA && resB)
  (L3, T, a :> b) -> do
    x <- get
    resA <- addF a >> prove
    resB <- put x >> addT b >> prove
    return (resA && resB)
  -- Update priority
  (i, _, _) | i < LOCK -> next h >> prove
  -- Search exhausted
  _ -> return False
