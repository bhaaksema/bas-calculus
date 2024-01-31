module Prover (sprove, iprove, cprove) where

import Embed
import Formula
import Sequent as S

data Logic = Int | Cl deriving Eq

-- | Prove a superintuitionistic theorem
sprove :: [Axiom] -> Formula -> Bool
sprove ax = iprove . embed ax

-- | Prove a intuitionistic theorem (m-G4ip)
iprove :: Formula -> Bool
iprove = prove Int . (<| []) . F . simply

-- | Prove a classical theorem (G3cp)
cprove :: Formula -> Bool
cprove = prove Cl . (<| []) . F . simply

-- | Check provability depending on the logic
prove :: Logic -> Sequent -> Bool
prove _ [] = False
prove l (e : x) = case e of
  -- Initial sequents
  (0, a) | a `elem` [T Bot, F Top] -> True
  -- Replacement rules
  (1, a) | a `elem` [T Top, F Bot] -> prove l x
  (1, T (Var s)) -> prove l $ S.substi s Top x
  (1, T (Var s :> Bot)) -> prove l $ S.substi s Bot x
  -- Unary premise rules
  (2, F (a :| b)) -> prove l $ F a <| F b <| x
  (2, F (a :> b)) | l == Cl || S.nullFs x
    -> prove l $ T a <| F b <| x
  (2, T (a :& b)) -> prove l $ T a <| T b <| x
  (2, T (c :& d :> b)) -> prove l $ T (c :> d :> b) <| x
  (2, T (c :| d :> b)) -> prove l $ T (c :> b) <| T (d :> b) <| x
  -- Binary premise rules
  (3, F (a :& b)) -> all (prove l) [F a <| x, F b <| x]
  (3, T (a :| b)) -> all (prove l) [T a <| x, T b <| F a <| x]
  (3, T (a :> b)) | l == Cl || S.nullFs x
    -> all (prove Cl) [F a <| x, T b <| x]
  -- Non-invertible rules
  (4, F (a :> b)) | prove l $ T a <| S.replaceFs b x -> True
  (4, T ((c :> d) :> b))
    | prove l $ T c <| T (d :> b) <| S.replaceFs d x
    -> prove l $ T b <| x
  -- Continue or fail
  (p, a) -> p < 5 && prove l (S.insert (succ p, a) x)
