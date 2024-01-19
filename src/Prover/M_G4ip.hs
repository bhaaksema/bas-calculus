{-# LANGUAGE LambdaCase #-}
module Prover.M_G4ip where

import           Formula     (Formula (..))
import           Multiset    ((+>))
import qualified Multiset    as M
import qualified Prover.G3cp as C

-- | Multi succedent sequent
type Sequent = (M.Multiset, M.Multiset)

-- | Prove a intuitionistic theorem
prove :: Formula -> Bool
prove a = prove1 (M.empty, M.singleton a)

-- | Check axioms and rules
prove1 :: Sequent -> Bool
prove1 (x, y)
  -- Initial sequent
  | M.vshare x y || M.unF x = True
  -- Glivenko's optimisation
  | y == M.singleton F = C.prove1 (x, y)
  -- Left conjunction
  | Just (a, b, x1) <- M.cget x = prove1 (a +> b +> x1, y)
  -- Right disjunction
  | Just (a, b, y1) <- M.dget y = prove1 (x, a +> b +> y1)
  -- Right implication
  | Just (a, b, y1) <- M.iget y, y1 == M.empty = prove1 (a +> x, b +> y1)
  -- Left implication
  | Just (V _, b, x1) <- iget = prove1 (b +> x1, y)
  | Just (F, _, x1) <- iget = prove1 (x1, y)
  | Just (c :& d, b, x1) <- iget = prove1 (c :> (d :> b) +> x1, y)
  | Just (c :| d, b, x1) <- iget = prove1 (c :> b +> d :> b +> x1, y)
  -- Right conjunction
  | Just (a, b, y1) <- M.cget y = prove1 (x, a +> y1) && (a == b || prove1 (x, b +> y1))
  -- Left disjunction (Weich's optimisation)
  | Just (a, b, x1) <- M.dget x = prove1 (a +> x1, y) && (a == b || prove1 (b +> x1, a +> y))
  -- Left implication (non-invertible)
  | Just (_, b, x1) <- M.ifind (\case
    e@(c :> d, b) -> prove1 (c +> d :> b +> M.idel e x, M.singleton d)
    _ -> False) x = prove1 (b +> x1, y)
  -- Right implication (non-invertible)
  | Just _ <- M.ifind (\case (a, b) -> prove1 (a +> x, M.singleton b)) y = True
  -- Failed to prove
  | otherwise = False where
  -- Get the conclusion of an invertible implication rule instance
  iget = M.ifind (\case (V s, _) -> V s `M.vmember` x; (_ :> _, _) -> False; _ -> True) x
