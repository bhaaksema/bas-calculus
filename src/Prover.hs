module Prover (prove) where

import Formula  (Formula (..))
import Multiset

handleGoal :: Multiset -> Formula -> Bool
handleGoal facts goal = case goal of
  Var v   -> v `elem` vars facts || handleFacts facts goal -- Initial Sequent
  T       -> True                                          -- Right Top
  And a b -> handleGoal facts a && handleGoal facts b      -- Right Conjunction
  Imp a b -> handleGoal (add a facts) b                    -- Right Implication
  _       -> handleFacts facts goal                        -- Left Rules

handleFacts :: Multiset -> Formula -> Bool
handleFacts facts goal
  | has bots facts = True -- Left Bottom
  | has tops facts = let  -- Left Top
      facts' = facts {tops = []}
    in handleGoal facts' goal
  | has ands facts = let  -- Left Conjunction
      (a, b) = head $ ands facts
      facts' = facts {ands = tail $ ands facts}
    in handleGoal (add [a, b] facts') goal
  | has ors facts = let   -- Left Disjunction
      (a, b) = head $ ors facts
      facts' = facts {ors = tail $ ors facts}
    in handleGoal (add a facts') goal
    && handleGoal (add b facts') goal
  | has imps facts = let  -- Left Implication
      (a, b) = head $ imps facts
      facts' = facts {imps = tail $ imps facts}
    in handleLImp facts' a b goal
  | otherwise = handleROr facts goal

handleLImp :: Multiset -> Formula -> Formula -> Formula -> Bool
handleLImp facts a b goal = case a of
  Var v   -> if v `elem` vars facts then handleGoal (add b facts) goal else handleROr facts goal
  F       -> handleGoal facts goal
  T       -> handleGoal (add b facts) goal
  And c d -> handleGoal (add (Imp c (Imp d b)) facts) goal
  Or c d  -> handleGoal (add [Imp c b, Imp d b] facts) goal
  Imp c d -> handleGoal (add (Imp c (Imp d b)) facts) goal
          || handleROr (add (Imp a b) facts) goal

handleROr :: Multiset -> Formula -> Bool
handleROr facts goal = case goal of
  Or a b -> handleGoal facts a || handleGoal facts b
  _      -> False -- No more rules to apply

prove :: [Formula] -> Formula -> Bool
prove _ = handleGoal defaultMultiset
