{-# LANGUAGE LambdaCase #-}
module Prover (iprove, cprove) where

import           Formula  (Formula (..))
import           Multiset ((+>))
import qualified Multiset as M

-- | Multi succedent sequent
type Sequent = (M.Multiset, M.Multiset)

-- | Prove a intuitionistic theorem (m-G4ip)
iprove :: Formula -> Bool
iprove a = prove False (M.empty, M.singleton a)

-- | Prove a classical theorem (G3cp)
cprove :: Formula -> Bool
cprove a = prove True (M.empty, M.singleton a)

-- | Check the sequent is provable depending on the logic
prove :: Bool -> Sequent -> Bool
prove l (x, y)
  -- Initial sequent
  | M.vshare x y || M.unF x = True
  -- Glivenko's optimisation
  | not l && y == M.singleton F = prove True (x, y)
  -- Left conjunction
  | Just (a, b, x1) <- M.cget x = prove l (a +> b +> x1, y)
  -- Right disjunction
  | Just (a, b, y1) <- M.dget y = prove l (x, a +> b +> y1)
  -- Right implication (classical and intuitionistic invertible)
  | Just (a, b, y1) <- M.iget y, l || y1 == M.empty = prove l (a +> x, b +> y1)
  -- Left implication (classical)
  | l, Just (a, b, x1) <- M.iget x = prove l (x1, a +> y) && prove l (b +> x1, y)
  -- Left implication (intuitionistic invertible)
  | Just (V s, b, x1) <- M.iget x, V s `M.vmember` x1 = prove l (b +> x1, y)
  | Just (F, _, x1) <- M.iget x = prove l (x1, y)
  | Just (c :& d, b, x1) <- M.iget x = prove l (c :> (d :> b) +> x1, y)
  | Just (c :| d, b, x1) <- M.iget x = prove l (c :> b +> d :> b +> x1, y)
  -- Right conjunction
  | Just (a, b, y1) <- M.cget y = prove l (x, a +> y1) && prove l (x, b +> y1)
  -- Left disjunction (Weich's optimisation)
  | Just (a, b, x1) <- M.dget x = prove l (a +> x1, y) && prove l (b +> x1, a +> y)
  -- Stash non-invertible candidates
  | Just (a, b, x1) <- M.iget x = prove l (M.stashOne (a :> b) x1, y)
  | Just (a, b, y1) <- M.iget y = prove l (x, M.stashOne (a :> b) y1)
  -- Move on to non-invertible candidates
  | otherwise = proveExv l (M.unstashAll x, M.unstashAll y)

proveExv :: Bool -> Sequent -> Bool
proveExv l (x, y)
  -- Left implication (intuitionistic non-invertible)
  | Just (c :> d, b, x1) <- M.iget x, prove l (c +> d :> b +> x1, M.singleton d) = prove l (b +> x1, y)
  | Just (a, b, x1) <- M.iget x = proveExv l (M.stashOne (a :> b) x1, y)
  -- Right implication (intuitionistic non-invertible)
  | Just (a, b, _) <- M.iget y, prove l (a +> x, M.singleton b) = True
  | Just (a, b, y1) <- M.iget y = proveExv l (x, M.stashOne (a :| b) y1)
  -- Failed to prove
  | otherwise = False
