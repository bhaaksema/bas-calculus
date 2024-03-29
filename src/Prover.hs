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
  (P0, T Bot) -> return True
  (P0, F Top) -> return True
  -- Unary consequence rules
  (P0, T (a :& b)) -> add (T a) >> add (T b) >> prove
  (P0, F (a :| b)) -> add (F a) >> add (F b) >> prove
  (P0, T ((c :& d) :> b)) -> add (T (c :> d :> b)) >> prove
  (P0, T ((c :| d) :> b)) -> freshVar >>= \p ->
    add (T (c :> p)) >> add (T (d :> p)) >> add (T (p :> b)) >> prove
  -- Replacement rules
  (P1, T (Var p)) -> mapSubsti True p Top >> prove
  (P1, F (Var p)) -> mapSubsti False p Bot >> inc f >> prove
  (P1, T (Var p :> Bot)) -> mapSubsti True p Bot >> prove
  -- Right implication
  (P2, F (a :> b)) -> do
    x <- get
    resA <- delFs >> add (T a) >> add (F b) >> prove
    resB <- put x >> inc f >> prove
    return (resA || resB)
  -- Right conjunction
  (P3, F (a :& b)) -> do
    x <- get
    resA <- add (F a) >> prove
    resB <- put x >> add (F b) >> prove
    return (resA && resB)
  -- Left disjunction
  (P3, T (a :| b)) -> do
    x <- get
    resA <- add (T a) >> prove
    resB <- put x >> add (T b) >> prove
    return (resA && resB)
  -- Left implication
  (P4, T ((c :> d) :> b)) -> do
    x <- get
    p <- freshVar
    add (T c); add (T (d :> b)); add (T (p :> b))
    resA <- delFs >> add (F p) >> prove
    resB <- put x >> add (T b) >> prove
    resC <- put x >> inc f >> prove
    return (if resA then resB else resC)
  -- Search exhausted
  (PMAX, _) -> return False
  -- Update priority
  _ -> inc f >> prove
