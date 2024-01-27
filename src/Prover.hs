module Prover (sprove, iprove, cprove) where

import Data.Maybe (isJust)

import Embed   (Axiom, embed)
import Formula (Formula (..))

import Multiset as M

data Logic = Int | Cl deriving Eq

-- | Prove a superintuitionistic theorem
sprove :: [Axiom] -> Formula -> Bool
sprove ax = iprove . embed ax

-- | Prove a intuitionistic theorem (m-G4ip)
iprove :: Formula -> Bool
iprove = prove0 Int . singleton . Right

-- | Prove a classical theorem (G3cp)
cprove :: Formula -> Bool
cprove = prove0 Cl . singleton . Right

-- | Check the sequent is provable depending on the logic
prove0 :: Logic -> Multiset -> Bool
prove0 Int s
  -- Glivenko's optimisation
  | rights s == [Bot] = prove0 Cl s
prove0 l s = case M.take s of
  -- Initial sequents
  Just (Left Bot, _) -> True; Just (Right Top, _) -> True
  -- Replace Left
  Just (Left (Var a), s1) -> prove0 l (M.fmap a Top s1)
  -- Replace Left-neg
  Just (Left (Var a :> Bot), s1) -> prove0 l ([Left Bot] +> M.fmap a Bot s1)
  -- Check next formula or move on to unary rules
  a -> (if isJust a then prove0 else prove1) l (M.iterate s)

-- | Helper function for unary rules
prove1 :: Logic -> Multiset -> Bool
prove1 l s = case M.take s of
  -- Left conjunction
  Just (Left (a :& b), s1) -> prove0 l ([Left a, Left b] +> s1)
  -- Right disjunction
  Just (Right (a :| b), s1) -> prove0 l ([Right a, Right b] +> s1)
  -- Right implication (classical)
  Just (Right (a :> b), s1) | l == Cl || null (rights s1)
    -> prove0 l ([Left a, Right b] +> s1)
  -- Check next formula or move on to binary rules
  a -> (if isJust a then prove1 else prove2) l (M.iterate s)

-- | Helper function for binary rules
prove2 :: Logic -> Multiset -> Bool
prove2 l s = case M.take s of
  -- Left disjunction (Weich's optimisation)
  Just (Left (a :| b), s1)
    -> prove0 l ([Left a] +> s1) && prove0 l ([Left b, Right a] +> s1)
  -- Left implication (classical)
  Just (Left (a :> b), s1) | l == Cl
    -> prove0 l ([Right a] +> s1) && prove0 l ([Left b] +> s1)
  -- Right conjunction
  Just (Right (a :& b), s1)
    -> prove0 l ([Right a] +> s1) && prove0 l ([Right b] +> s1)
  -- Check next formula or move on ton on-invertible rules
  a -> (if isJust a then prove2 l else prove3) (M.iterate s)

-- | Helper function for non-invertible rules
prove3 :: Multiset -> Bool
prove3 s = case M.take s of
  -- Left implication (intuitionistic)
  Just (Left ((c :> d) :> b), s1)
    | prove0 Int ([Left c, Left (d :> b)] +> setRight d s1)
    -> prove0 Int ([Left b] +> s1)
  -- Right implication (intuitionistic)
  Just (Right (a :> b), s1)
    | prove0 Int ([Left a] +> setRight b s1) -> True
  -- Check next formula or fail
  a -> isJust a && prove3 (M.iterate s)
