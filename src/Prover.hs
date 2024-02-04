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
  (0, T Bot) -> True; (0, F Top) -> True
  -- Replacement rules
  (0, T Top) -> prove l x; (0, F Bot) -> prove l x
  (1, T (Var p)) -> prove l $ substi p Top x
  (1, T (Var p :> Bot)) -> prove l $ substi p Bot x
  -- Unary premise rules
  (2, T (a :& b)) -> prove l $ T a <|^ T b <|^ x
  (2, F (a :| b)) -> prove l $ F a <|^ F b <|^ x
  (3, F (a :> b))
    | l == Cl || nullFs x -> prove l $ T a <|^ F b <|^ x
    | prove l $ T a <|^ replaceFs b x -> True
  -- Binary premise rules
  (4, F (a :& b)) -> all (prove l) [F a <|^ x, F b <|^ x]
  (4, T (a :| b)) -> all (prove l) [T a <|^ x, T b <|^ F a <|^ x]
  -- Non-invertible rules
  (5, T (a@(c :> d) :> b))
    | l == Cl || nullFs x -> all (prove Cl) [F a <|^ x, T b <|^ x]
    | not (prove l $ T b <|^ x) -> False
    | prove l $ T c <|^ T (d :> b) <|^ replaceFs d x -> True
  -- Iterate the sequent
  (i, a) -> i < 6 && prove l (insert (succ i, a) x)
  -- Search exhausted
  | otherwise = False
