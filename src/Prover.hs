module Prover (sprove, iprove, cprove) where

import Embed
import Formula
import Sequent

-- | Ruleset is based on the logic
data Logic = Int | Cl deriving Eq

-- | Prove a superintuitionistic theorem
sprove :: [Axiom] -> Formula -> Bool
sprove ax = iprove . embed ax

-- | Prove a intuitionistic theorem (m-G4ip)
iprove :: Formula -> Bool
iprove = prove Int . singleton . F . simply

-- | Prove a classical theorem (G3cp)
cprove :: Formula -> Bool
cprove = prove Cl . singleton . F . simply

-- | Check provability depending on the logic
prove :: Logic -> Sequent -> Bool
prove l y | Just (e, x) <- view y = case e of
  -- Initial sequents
  T Bot -> True; F Top -> True
  -- Replacement rules
  T (Var p) -> prove l (substi p Top x)
  T (Var p :> Bot) -> prove l (substi p Bot x)
  -- Unary premise rules
  F (a :> b)
    | l == Cl || nullFs x -> prove l (T a +> F b +> x)
    | prove l (T a +> replaceFs b x) -> True
  -- Binary premise rules
  F (a :& b) -> all (prove l) [F a +> x, F b +> x]
  T (a :| b) -> all (prove l) [T a +> x, T b +> F a +> x]
  -- Non-invertible rules
  T (a@(c :> d) :> b)
    | l == Cl || nullFs x -> all (prove Cl) [F a +> x, T b +> x]
    | not (prove l (T b +> x)) -> False
    | prove l (T c +> T (d :> b) +> replaceFs d x) -> True
  -- Backtracking
  a -> prove l (x <+ a)
  -- Search exhausted
  | otherwise = False
