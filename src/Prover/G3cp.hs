module Prover.G3cp where

import           Formula
import           MultiSet ((>.))
import qualified MultiSet as M

-- | Multi succedent sequent
type Sequent = (M.MultiSet, M.MultiSet)

-- | Prove a classical theorem
prove :: Formula -> Bool
prove a = prove1 (M.empty, M.singleton a)

-- | Check axioms and rules
prove1 :: Sequent -> Bool
prove1 (x, y)
  | M.vshare x y || M.unF x || M.unT y = True
  | Just (a :& b, x1) <- M.pop isAnd x = prove1 (a >. b >. x1, y)
  | Just (a :| b, y1) <- M.pop isOr  y = prove1 (x, a >. b >. y1)
  | Just (a :> b, y1) <- M.pop isImp y = prove1 (a >. x, b >. y1)
  | Just (a :& b, y1) <- M.pop isAnd y = prove1 (x, a >. y1) && ((a /= b) && prove1 (x, b >. y1))
  | Just (a :| b, x1) <- M.pop isOr  x = prove1 (a >. x1, y) && ((a /= b) && prove1 (b >. x1, y))
  | Just (a :> b, x1) <- M.pop isImp x = prove1 (x1, a >. y) && prove1 (b >. x1, y)
  | otherwise = False
