module Prover.G3cp where

import           Formula
import           MultiSet ((<.), (>.))
import qualified MultiSet as M

-- | Multi succedent sequent
type Sequent = (M.MultiSet, M.MultiSet)

-- | Prove a classical theorem
prove :: Formula -> Bool
prove a = prove1 (M.empty, M.singleton a)

-- | Check axioms and rules
prove1 :: Sequent -> Bool
prove1 (x, y)
  -- Initial sequent
  | M.vshare x y || M.unF x || M.unT y = True
  -- Left conjunction
  | Just (a :& b, x1) <- isA <. x = prove1 (a >. b >. x1, y)
  -- Right disjunction
  | Just (a :| b, y1) <- isO <. y = prove1 (x, a >. b >. y1)
  -- Right implication
  | Just (a :> b, y1) <- isI <. y = prove1 (a >. x, b >. y1)
  -- Right conjunction
  | Just (a :& b, y1) <- isA <. y = prove1 (x, a >. y1) && prove1 (x, b >. y1)
  -- Left disjunction
  | Just (a :| b, x1) <- isO <. x = prove1 (a >. x1, y) && prove1 (b >. x1, y)
  -- Left implication
  | Just (a :> b, x1) <- isI <. x = prove1 (x1, a >. y) && prove1 (b >. x1, y)
  | otherwise = False
