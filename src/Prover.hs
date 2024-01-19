{-# LANGUAGE LambdaCase #-}
module Prover (sprove, iprove, cprove) where

import qualified Bounding as B
import           Formula  (Formula (..))
import           Multiset ((+>))
import qualified Multiset as M

-- | Multi succedent sequent
type Sequent = (M.Multiset, M.Multiset)

-- | Prove a superintuitionistic theorem (m-G4ip)
sprove :: [B.Axiom] -> Formula -> Bool
sprove as f = iprove $ foldl1 (:&) (B.for as f) :> f

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
  | Just (V _, b, x1) <- iget = prove l (b +> x1, y)
  | Just (F, _, x1) <- iget = prove l (x1, y)
  | Just (c :& d, b, x1) <- iget = prove l (c :> (d :> b) +> x1, y)
  | Just (c :| d, b, x1) <- iget = prove l (c :> b +> d :> b +> x1, y)
  -- Right conjunction
  | Just (a, b, y1) <- M.cget y = prove l (x, a +> y1) && (a == b || prove l (x, b +> y1))
  -- Left disjunction (Weich's optimisation)
  | Just (a, b, x1) <- M.dget x = prove l (a +> x1, y) && (a == b || prove l (b +> x1, a +> y))
  -- Left implication (intuitionistic non-invertible)
  | Just (_, b, x1) <- M.ifind (\case
    e@(c :> d, b) -> prove l (c +> d :> b +> M.idel e x, M.singleton d)
    _ -> False) x = prove l (b +> x1, y)
  -- Right implication (intuitionistic non-invertible)
  | Just _ <- M.ifind (\case(a, b) -> prove l (a +> x, M.singleton b)) y = True
  -- Failed to prove
  | otherwise = False where
  -- Get the conclusion of an invertible implication rule instance
  iget = M.ifind (\case (V s, _) -> V s `M.vmember` x; (_ :> _, _) -> False; _ -> True) x
