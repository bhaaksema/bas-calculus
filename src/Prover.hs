module Prover (sprove, iprove, cprove) where

import Control.Monad.State
import GHC.Conc

import Embed
import Formula
import State

-- | Prove a classical theorem
cprove :: Formula -> Bool
cprove = iprove . neg . neg

-- | Prove a superintuitionistic theorem
sprove :: [Axiom] -> Formula -> Bool
sprove ax = iprove . embed ax

-- | Prove a intuitionistic theorem
iprove :: Formula -> Bool
iprove = evalState prove . newState . simply

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
  (L1, F, Var p) -> subst False p Bot >> inc h >> prove
  (L1, T, (Var p) :> Bot) -> subst True p Bot >> prove
  -- Unary consequence rules
  (L2, T, a :& b) -> addT a >> addT b >> prove
  (L2, F, a :| b) -> addF a >> addF b >> prove
  (L2, T, (a :& b) :> c) -> addT (a :> b :> c) >> prove
  (L2, T, (a :| b) :> c) -> freshV >>= \p ->
    addT (a :> p) >> addT (b :> p) >> addT (p :> c) >> prove
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
  (L4, F, a :> b) -> do
    x <- get
    resA <- addT a >> setF b >> reset >> prove
    resB <- put x >> inc h >> prove
    return (resA `par` resB `pseq` resA || resB)
  -- Ternary consequence rules
  (L5, T, (a :> b) :> c) -> do
    x <- get
    resA <- addT a >> freshV >>= \p ->
      addT (b :> p) >> addT (p :> c) >> setF p >> reset >> prove
    resB <- put x >> addT c >> reset >> prove
    resC <- put x >> inc h >> prove
    return (if resA then resB else resC)
  -- Update priority
  (i, _, _) | i < maxBound -> inc h >> prove
  -- Search exhausted
  _ -> return False
