module Prover (iprove, cprove) where

import           Formula  (Formula (..))
import           Multiset ((+>))
import qualified Multiset as M

-- | Multi succedent sequent
type Sequent = (M.Multiset, M.Multiset)

-- | Prove a intuitionistic theorem (m-G4ip)
iprove :: Formula -> Bool
iprove a = prove True (M.empty, M.singleton a)

-- | Prove a classical theorem (G3cp)
cprove :: Formula -> Bool
cprove a = prove False (M.empty, M.singleton a)

-- | Check the sequent is provable with any rule
prove :: Bool -> Sequent -> Bool
prove i (x, y)
  -- Initial sequent
  | M.vshare x y || M.bot x || M.top y = True
  -- Glivenko's optimisation
  | i, y == M.singleton Bot = prove False (x, y)
  -- Left conjunction
  | Just (a :& b, x1) <- M.bpop x = prove i (a +> b +> x1, y)
  -- Left implication (intuitionistic)
  | i, Just (a :> b, x1) <- M.bpop x = case a of
    Bot    -> prove i (x1, y)
    Top    -> prove i (b +> x1, y)
    c :& d -> prove i (c :> (d :> b) +> x1, y)
    c :| d -> prove i (c :> b +> d :> b +> x1, y)
    _      -> prove i (M.bdown x, y)
  -- Right disjunction
  | Just (a :| b, y1) <- M.bpop y = prove i (x, a +> b +> y1)
  -- Right implication
  | Just (a :> b, y1) <- M.bpop y = if not i || y1 == M.empty
    then prove i (a +> x, b +> y1) else prove i (x, M.bdown y)
  -- Continue with non-unary invertible rules
  | otherwise = prove1 i (M.bceil x, M.bceil y)

-- | Check the sequent is provable with non-unary rules
prove1 :: Bool -> Sequent -> Bool
prove1 i (x, y)
  -- Left implication (intuitionistic cont.)
  | i, Just (a :> b, x1) <- M.bpop x = case a of
    Var s | s `M.vmember` x1 -> prove i (b +> x1, y)
    _                        -> prove1 i (M.bdown x, y)
  -- Left disjunction (Weich's optimisation)
  | Just (a :| b, x1) <- M.bpop x = all (prove i) [(a +> x1, y), (b +> x1, a +> y)]
  -- Left implication (classical)
  | Just (a :> b, x1) <- M.bpop x = all (prove i) [(x1, a +> y), (b +> x1, y)]
  -- Right conjunction
  | Just (a :& b, y1) <- M.bpop y = all (prove i) [(x, a +> y1), (x, b +> y1)]
  -- Continue with non-invertible rules
  | otherwise = prove2 i (M.bceil x, M.bceil y)

-- | Check the sequent is provable with non-invertible rules
prove2 :: Bool -> Sequent -> Bool
prove2 i (x, y)
  -- Left implication (intuitionistic cont.)
  | Just (a :> b, x1) <- M.bpop x = case a of
    c :> d | prove i (c +> d :> b +> x1, M.singleton d) -> prove i (b +> x1, y)
    _ -> prove2 i (M.bdown x, y)
  -- Right implication (intuitionistic)
  | Just (a :> b, _) <- M.bpop y = prove i (a +> x, M.singleton b) || prove2 i (x, M.bdown y)
  -- Failed to prove
  | otherwise = False
