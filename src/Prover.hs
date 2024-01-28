module Prover (sprove, iprove, cprove) where

import Embed   (Axiom, embed)
import Formula
import Sequent

data Logic = Int | Cl deriving Eq

-- | Prove a superintuitionistic theorem
sprove :: [Axiom] -> Formula -> Bool
sprove ax = iprove . embed ax

-- | Prove a intuitionistic theorem (m-G4ip)
iprove :: Formula -> Bool
iprove = prove Int . singleton . Right

-- | Prove a classical theorem (G3cp)
cprove :: Formula -> Bool
cprove = prove Cl . singleton . Right

-- | Check the sequent is provable depending on the logic
prove :: Logic -> Sequent -> Bool
prove l xs = let x = tail xs in case head xs of
  -- Initial sequents
  (0, Left Bot) -> True; (0, Right Top) -> True
  -- Glivenko's optimisation (intuitionistic)
  (_, Right Bot) | l == Int, null (rights x) -> prove Cl xs
  -- Replace Left
  (1, Left (Var s)) -> prove l (Top `left` sub (s, Top) x)
  -- Replace Left-neg
  (1, Left (Var s :> Bot)) -> prove l (Bot `left` sub (s, Bot) x)
  -- Left conjunction
  (2, Left (a :& b)) -> prove l (a `left` b `left` x)
  -- Right disjunction
  (2, Right (a :| b)) -> prove l (a `right` b `right` x)
  -- Right implication (classical)
  (2, Right (a :> b)) | l == Cl || null (rights x)
    -> prove l (a `left` b `right` x)
  -- Left disjunction (Weich's optimisation)
  (3, Left (a :| b)) -> prove l (a `left` x) && prove l (b `left` a `right` x)
  -- Left implication (classical)
  (3, Left (a :> b)) | l == Cl -> prove l (a `right` x) && prove l (b `left` x)
  -- Right conjunction
  (3, Right (a :& b)) -> prove l (a `right` x) && prove l (b `right` x)
  -- Left implication (intuitionistic)
  (4, Left ((c :> d) :> b)) | l == Int, prove l (c `left` d :> b `left` d `setRight` x)
    -> prove l (b `left` x)
  -- Right implication (intuitionistic)
  (4, Right (a :> b)) | l == Int, prove l (a `left` b `setRight` x) -> True
  -- Reorder formulae in the sequent
  (p, a) -> (p < 5) && prove l ((succ p, a) `insert` x)
