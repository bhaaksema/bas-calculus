module Prover (sprove, iprove, cprove) where

import Control.Monad.State

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
iprove = evalState prove . singletonF . simply

-- | Check provability
prove :: State ProverState Bool
prove = view >>= \f -> case f of
  -- Contradictory leaf
  (L0, T, Bot) -> return True
  (L0, F, Top) -> return True
  -- Replacement rules
  (L1, T, Var p) -> mapSubsti True p Top >> prove
  (L1, F, Var p) -> mapSubsti False p Bot >> inc f >> prove
  (L1, T, Var p :> Bot) -> mapSubsti True p Bot >> prove
  -- Unary consequence rules
  (L2, T, a :& b) -> addT a >> addT b >> prove
  (L2, F, a :| b) -> addF a >> addF b >> prove
  (L2, T, (c :& d) :> b) -> addT (c :> d :> b) >> prove
  (L2, T, (c :| d) :> b) -> freshV >>= \p ->
    addT (c :> p) >> addT (d :> p) >> addT (p :> b) >> prove
  -- Binary consequence rules
  (L3, F, a :& b) -> do
    x <- get
    resA <- addF a >> prove
    put x
    resB <- addF b >> prove
    return (resA && resB)
  (L3, T, a :| b) -> do
    x <- get
    resA <- addT a >> prove
    put x
    resB <- addT b >> prove
    return (resA && resB)
  (L3, F, a :> b) -> do
    x <- get
    resA <- addT a >> setF b >> prove
    put x
    resB <- inc f >> prove
    return (resA || resB)
  -- Ternary consequence rules
  (L4, T, (c :> d) :> b) -> do
    x <- get
    p <- freshV
    addT c; addT (d :> b); addT (p :> b)
    resA <- setF p >> prove
    put x
    resB <- addT b >> prove
    put x
    resC <- inc f >> prove
    return (if resA then resB else resC)
  -- Update priority
  (i, _, _) | i < maxBound -> inc f >> prove
  -- Search exhausted
  _ -> return False
