module Prover (sprove, iprove, cprove) where

import Embed
import Formula
import Sequent

-- | Prove a superintuitionistic theorem
sprove :: [Axiom] -> Formula -> Bool
sprove ax = iprove . embed ax

-- | Prove a intuitionistic theorem
iprove :: Formula -> Bool
iprove = prove . singleton . F . simply

-- | Prove a classical theorem
cprove :: Formula -> Bool
cprove = prove . singleton . F . simply . neg . neg

-- | Check provability of the set
prove :: Sequent -> Bool
prove y | Just (e, x) <- view y = case e of
  -- Contradictory
  (P0, T Bot) -> True; (P0, F Top) -> True
  -- Unary consequence rules
  (P0, T (a :& b)) -> prove (T a <| T b <| x)
  (P0, F (a :| b)) -> prove (F a <| F b <| x)
  (P0, T ((c :& d) :> b)) -> prove (T (c :> d :> b) <| x)
  (P0, T ((c :| d) :> b)) -> prove (T (c :> b) <| T (d :> b) <| x)
  -- Replacement rules
  (P1, T (Var p)) -> prove (mapSubsti True p Top x)
  (P1, F (Var p)) -> prove (e <+ mapSubsti False p Bot x)
  (P1, T (Var p :> Bot)) -> prove (mapSubsti True p Bot x)
  -- Right implication
  (P2, F (a :> b)) | prove (T a <| F b <| delFs x) -> True
  -- Right conjunction
  (P3, F (a :& b)) -> all prove [F a <| x, F b <| x]
  -- Left disjunction
  (P3, T (a :| b)) -> all prove [T a <| x, T b <| F a <| x]
  -- Left implication
  (P4, T ((c :> d) :> b))
    | prove (T c <| T (d :> b) <| F d <| delFs x) -> prove (T b <| x)
  -- Update priority
  a -> prove (a <+ x)
  -- Search exhausted
  | otherwise = False
