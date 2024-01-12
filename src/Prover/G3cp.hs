{-# LANGUAGE LambdaCase #-}
module Prover.G3cp where

import           Formula
import           MultiSet ((+>))
import qualified MultiSet as M

-- | Multi succedent sequent
type Sequent = (M.MultiSet, M.MultiSet)

-- | Get conjunction
getC :: M.MultiSet -> Maybe (Formula, M.MultiSet)
getC = M.pop $ \case (_ :& _) -> True; _ -> False

-- | Get disjunction
getD :: M.MultiSet -> Maybe (Formula, M.MultiSet)
getD = M.pop $ \case (_ :| _) -> True; _ -> False

-- | Check if implication
isI :: Formula -> Bool
isI = \case (_ :> _) -> True; _ -> False

-- | Get implication
getI :: M.MultiSet -> Maybe (Formula, M.MultiSet)
getI = M.pop isI

-- | Prove a classical theorem
prove :: Formula -> Bool
prove a = prove1 (M.empty, M.singleton a)

-- | Check axioms and rules
prove1 :: Sequent -> Bool
prove1 (x, y)
  -- Initial sequent
  | M.vshare x y || M.unF x || M.unT y = True
  -- Left conjunction
  | Just (a :& b, x1) <- getC x = prove1 (a +> b +> x1, y)
  -- Right disjunction
  | Just (a :| b, y1) <- getD y = prove1 (x, a +> b +> y1)
  -- Right implication
  | Just (a :> b, y1) <- getI y = prove1 (a +> x, b +> y1)
  -- Right conjunction
  | Just (a :& b, y1) <- getC y = prove1 (x, a +> y1) && prove1 (x, b +> y1)
  -- Left disjunction
  | Just (a :| b, x1) <- getD x = prove1 (a +> x1, y) && prove1 (b +> x1, y)
  -- Left implication
  | Just (a :> b, x1) <- getI x = prove1 (x1, a +> y) && prove1 (b +> x1, y)
  | otherwise = False
