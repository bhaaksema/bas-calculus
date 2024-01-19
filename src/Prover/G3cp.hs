module Prover.G3cp where

import           Formula  (Formula)
import           Multiset ((+>))
import qualified Multiset as M

-- | Multi succedent sequent
type Sequent = (M.Multiset, M.Multiset)

-- | Prove a classical theorem
prove :: Formula -> Bool
prove a = prove1 (M.empty, M.singleton a)

-- | Check axioms and rules
prove1 :: Sequent -> Bool
prove1 (x, y)
  -- Initial sequent
  | M.vshare x y || M.unF x || M.unT y = True
  -- Left conjunction
  | Just (a, b, x1) <- M.cget x = prove1 (a +> b +> x1, y)
  -- Right disjunction
  | Just (a, b, y1) <- M.dget y = prove1 (x, a +> b +> y1)
  -- Right implication
  | Just (a, b, y1) <- M.iget y = prove1 (a +> x, b +> y1)
  -- Right conjunction
  | Just (a, b, y1) <- M.cget y = prove1 (x, a +> y1) && (a == b || prove1 (x, b +> y1))
  -- Left disjunction
  | Just (a, b, x1) <- M.dget x = prove1 (a +> x1, y) && (a == b || prove1 (b +> x1, y))
  -- Left implication
  | Just (a, b, x1) <- M.iget x = prove1 (x1, a +> y) && prove1 (b +> x1, y)
  -- Failed to prove
  | otherwise = False
