{-# LANGUAGE LambdaCase #-}
module Prover (iprove, cprove) where

import           Formula  (Formula (..))
import qualified Formula  as F
import           Multiset ((+>))
import qualified Multiset as M

-- | Multi succedent sequent
type Sequent = (M.Multiset, M.Multiset)

-- | Create a sequent from a formula
sequent :: Formula -> Sequent
sequent a = (M.empty, M.singleton $ F.simplify a)

-- | Prove a intuitionistic theorem (m-G4ip)
iprove :: Formula -> Bool
iprove a = prove True $ sequent a

-- | Prove a classical theorem (G3cp)
cprove :: Formula -> Bool
cprove a = prove False $ sequent a

-- | Check the sequent is provable depending on the logic
prove :: Bool -> Sequent -> Bool
prove i (x, y)
  -- Initial sequent
  | M.vshare x y || M.bot x || M.top y = True
  -- Glivenko's optimisation
  | i, y == M.singleton Bot = prove False (x, y)
  -- Right disjunction
  | Just (a, b, y1) <- M.dget y = prove i (x, a +> b +> y1)
  -- Left conjunction
  | Just (a, b, x1) <- M.cget x = prove i (a +> b +> x1, y)
  -- Right implication (invertible)
  | Just (a, b, y1) <- M.iget y, not i || y1 == M.empty = prove i (a +> x, b +> y1)
  -- Left implication (invertible)
  | not i, Just (a, b, x1) <- M.iget x = prove i (x1, a +> y) && prove i (b +> x1, y)
  | Just (Var _, b, x1) <- iget = prove i (b +> x1, y)
  | Just (Bot, _, x1) <- iget = prove i (x1, y)
  | Just (Top, b, x1) <- iget = prove i (b +> x1, y)
  | Just (c :& d, b, x1) <- iget = prove i (c :> (d :> b) +> x1, y)
  | Just (c :| d, b, x1) <- iget = prove i (c :> b +> d :> b +> x1, y)
  -- Right conjunction
  | Just (a, b, y1) <- M.cget y = prove i (x, a +> y1) && prove i (x, b +> y1)
  -- Left disjunction (Weich's optimisation)
  | Just (a, b, x1) <- M.dget x = prove i (a +> x1, y) && prove i (b +> x1, a +> y)
  -- Right implication (non-invertible)
  | Just _ <- M.ifind (\case(a, b) -> prove i (a +> x, M.singleton b)) y = True
  -- Left implication (non-invertible)
  | Just (_, b, x1) <- M.ifind (\case
    e@(c :> d, b) -> prove i (c +> d :> b +> M.idel e x, M.singleton d)
    _ -> False) x = prove i (b +> x1, y)
  -- Failed to prove
  | otherwise = False
  -- Get the conclusion of an invertible implication rule instance
  where iget = M.ifind (\case (Var s, _) -> s `M.vmember` x; (_ :> _, _) -> False; _ -> True) x
