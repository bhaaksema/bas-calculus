module Prover (sprove, iprove, cprove) where

import qualified Multiset as M

import Embed   (Axiom, embed)
import Formula (Formula (..))
import Sequent as S

data Logic = Int | Cl deriving Eq

-- | Prove a superintuitionistic theorem
sprove :: [Axiom] -> Formula -> Bool
sprove ax = iprove . embed ax

-- | Prove a intuitionistic theorem (m-G4ip)
iprove :: Formula -> Bool
iprove = prove Int . singletonRight

-- | Prove a classical theorem (G3cp)
cprove :: Formula -> Bool
cprove = prove Cl . singletonRight

-- | Check the sequent is provable depending on the logic
prove :: Logic -> Sequent -> Bool
prove l s@(Sequent _ x y)
  -- Initial sequent
  | M.bot x || M.top y = True
  -- Glivenko's optimisation
  | l == Int, y == M.singleton Bot = prove Cl s
  -- Right disjunction
  | Just (a :| b, s1) <- takeRight s = prove l (a +> b +> s1)
  -- Left conjunction
  | Just (a :& b, s1) <- takeLeft s = prove l (a +< b +< s1)
  -- Right implication
  | Just (a :> b, s1) <- takeRight s
  , l == Cl || y == M.empty = prove l (a +< b +> s1)
  -- Left implication (intuitionistic)
  | Just (c :& d :> b, s1) <- takeLeft s = prove l (c :> (d :> b) +< s1)
  | Just (c :| d :> b, s1) <- takeLeft s = prove l (c :> b +< d :> b +< s1)
  -- Check next formula
  | Just s1 <- S.iterate s = prove l s1
  -- Move on to binary rules
  | otherwise = prove1 l (S.reset s)

-- | Helper function for binary rules
prove1 :: Logic -> Sequent -> Bool
prove1 l s
  -- Right conjunction
  | Just (a :& b, s1) <- takeRight s = prove l (a +> s1) && prove l (b +> s1)
  -- Left disjunction (Weich's optimisation)
  | Just (a :| b, s1) <- takeLeft s = prove l (a +< s1) && prove l (b +< a +> s1)
  -- Left implication (classical)
  | l == Cl, Just (a :> b, s1) <- takeLeft s = prove l (a +> s1) && prove l (b +< s1)
  -- Check next formula
  | Just s1 <- S.iterate s = prove1 l s1
  -- Move on to non-invertible rules
  | otherwise = prove2 (S.reset s)

-- | Helper function for non-invertible rules
prove2 :: Sequent -> Bool
prove2 s
  -- Right implication
  | Just (a :> b, s1) <- takeRight s
  , prove Int (a +< s1 {right = M.singleton b}) = True
  -- Left implication
  | Just ((c :> d) :> b, s1) <- takeLeft s
  , prove Int (c +< d :> b +< s1 {right = M.singleton d}) = prove Int (b +< s1)
  -- Check next formula
  | Just s1 <- S.iterate s = prove2 s1
  -- Failed to prove
  | otherwise = False
