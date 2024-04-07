module Prover.Intuition (iprove, prove) where

import Control.Monad.State

import           Formula
import qualified Prover.Classic as C
import           State


-- | Prove a intuitionistic theorem
iprove :: Formula -> Bool
iprove = evalState prove . newState . simply

-- | Check provability
prove :: State ProverState Bool
prove = view >>= \f -> case f of
  -- Contradictory leaf
  (L0, T, Bot) -> return True
  (L0, F, Top) -> return True
  -- Replacement rules
  (L0, T, Top) -> prove
  (L0, F, Bot) -> nullFs >>= \z ->
    if z then unlock >> C.prove else prove
  (L1, T, Var p) -> subst True p Top >> prove
  (L1, F, Var p) -> subst False p Bot >> lock f >> prove
  (L1, T, (Var p) :> Bot) -> subst True p Bot >> prove
  -- Unary consequence rules
  (L2, T, a :& b) -> addT a >> addT b >> prove
  (L2, F, a :| b) -> addF a >> addF b >> prove
  (L2, T, (a :& b) :> c) -> addT (a :> b :> c) >> prove
  (L2, T, (a :| b) :> c) -> freshV >>= \p ->
    addT (a :> p) >> addT (b :> p) >> addT (p :> c) >> prove
  -- Binary consequence rules
  (L3, T, a :| b) -> do
    x <- get
    resA <- addT a >> prove
    resB <- put x >> addT b >> prove
    return (resA && resB)
  (L3, F, a :& b) -> do
    x <- get
    resA <- addF a >> prove
    resB <- put x >> addF b >> prove
    return (resA && resB)
  (L3, F, a :> b) -> do
    x <- get
    resA <- put x >> addT a >> setF b >> prove
    resB <- put x >> lock f >> prove
    return (resA || resB)
  -- Ternary consequence rules
  (L4, T, (a :> b) :> c) -> do
    x <- get
    resA <- unlock >> addT a >> freshV >>= \p ->
      addT (b :> p) >> addT (p :> c) >> setF p >> prove
    resB1 <- put x >> unlock >> addT c >> C.prove
    resB2 <- put x >> unlock >> addT c >> prove
    resC <- put x >> lock f >> prove
    return (if resA then resB1 && resB2 else resC)
  -- Update priority
  (i, _, _) | i < LOCK -> next f >> prove
  -- Search exhausted
  _ -> return False
