module Prover (sprove, iprove, cprove) where

import Data.Sequence hiding ((:>), (<|))

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
iprove = prove Int . single . F . simply

-- | Prove a classical theorem (G3cp)
cprove :: Formula -> Bool
cprove = prove Cl . single . F . simply

-- | Check provability depending on the logic
prove :: Logic -> Sequent -> Bool
prove _ Empty = False
prove l (e :<| x) = case e of
  -- Initial sequents
  (0, T Bot) -> True; (0, F Top) -> True
  -- Replacement rules
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
  -- Continue
  (i, a) -> i < 6 && prove l (sortOn fst ((succ i, a) :<| x))
