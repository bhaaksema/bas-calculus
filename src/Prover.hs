module Prover (sprove, iprove, cprove) where

import qualified Data.Set as S

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
prove l y = case S.minView y of
  Just (e, x) -> case e of
    -- Initial sequents
    (0, a) | a `elem` [T Bot, F Top] -> True
    -- Replacement rules
    (0, T (Var s)) -> prove l $ substi s Top x
    (0, T (Var s :> Bot)) -> prove l $ substi s Bot x
    -- Unary premise rules
    (0, T (a :& b)) -> prove l $ T a <| T b <| x
    (0, F (a :| b)) -> prove l $ F a <| F b <| x
    (0, F (a :> b)) | l == Cl || nullFs x
      -> prove l $ T a <| F b <| x
    (0, T ((c :& d) :> b)) -> prove l $ T (c :> d :> b) <| x
    (0, T ((c :| d) :> b)) -> prove l $ T (c :> b) <| T (d :> b) <| x
    -- Binary premise rules
    (1, F (a :& b)) -> all (prove l) [F a <| x, F b <| x]
    (1, T (a :| b)) -> all (prove l) [T a <| x, T b <| F a <| x]
    (1, T (a :> b)) | l == Cl || nullFs x
      -> all (prove Cl) [F a <| x, T b <| x]
    -- Non-invertible rules
    (2, F (a :> b)) | prove l $ T a <| replaceFs b x -> True
    (2, T ((c :> d) :> b))
      | prove l $ T c <| T (d :> b) <| replaceFs d x
      -> prove l $ T b <| x
    -- Continue or fail
    (p, a) -> p < 3 && prove l (S.insert (succ p, a) x)
  Nothing -> False
