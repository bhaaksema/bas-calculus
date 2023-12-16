module Prover (prove) where

import Formula  (Formula (..))
import Multiset

-- | handleGoal function takes a multiset of facts and a goal formula, and
--   recursively analyses the goal formula based on the available facts.
handleGoal :: Multiset -> Formula -> Bool
handleGoal facts goal = case goal of
  T       -> True
  And a b -> handleGoal facts a && handleGoal facts b
  Imp a b -> handleGoal (add a facts) b
  _       -> handleFacts facts goal

handleFacts :: Multiset -> Formula -> Bool
handleFacts facts goal
  | has bots facts = True
  | has tops facts = handleGoal (facts {tops = []}) goal
  | has ands facts = let
      (a, b) = head $ ands facts
      facts' = facts {ands = tail $ ands facts}
    in handleGoal (add [a, b] facts') goal
  | has ors facts = let
      (a, b) = head $ ors facts
      facts' = facts {ors = tail $ ors facts}
    in handleGoal (add a facts') goal && handleGoal (add b facts') goal
  | has imps facts = let
      (a, b) = head $ imps facts
      facts' = facts {imps = tail $ imps facts}
    in handleImpFacts facts' a b goal
  | otherwise = handleBranch facts goal

handleImpFacts :: Multiset -> Formula -> Formula -> Formula -> Bool
handleImpFacts facts a b goal = case a of
  Var v   -> v `elem` vars facts && handleGoal (add b facts) goal
  F       -> handleGoal facts goal
  T       -> handleGoal (add b facts) goal
  And c d -> handleGoal (add (Imp c (Imp d b)) facts) goal
  Or c d  -> handleGoal (add [Imp c b, Imp d b] facts) goal
  _       -> handleBranch facts goal

handleBranch :: Multiset -> Formula -> Bool
handleBranch _ _ = True
-- TODO
-- Initial Sequent
-- Right Disjunction
-- Left Compound Implication

-- | Prove function takes a list of axioms and a goal formula, and returns
--   whether the goal formula can be proven using the given axioms.
prove :: [Formula] -> Formula -> Bool
prove _ = handleGoal defaultMultiset
