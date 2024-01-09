module Prover.G4ip where

import           Formula
import           MultiSet
import qualified Prover.G3cp as C

type Sequent = (MultiSet, Formula)

-- Prove a formula
prove :: Formula -> Bool
prove a = prove1 (empty, a)

-- Prove a sequent
prove1 :: Sequent -> Bool
prove1 s = axiom s || rule s

-- Check if the sequent satisfies an axiom
axiom :: Sequent -> Bool
axiom (x, y)
  | Var v <- y, v `member` x = True
  | otherwise = unF x || y == T

-- Check if the sequent satisfies a rule
rule :: Sequent -> Bool
rule (x, y)
  | F <- y = C.prove1 (x, singleton F)
  | a :> b <- y = prove1 (a >. x, b)
  | Just (a :& b, x1) <- pop isAnd x = prove1 (a >. b >. x1, y)
  | Just (Var v :> b, x1) <- pop isVImp x, v `member` x = prove1 (b >. x1, y)
  | Just (F :> _, x1) <- pop isOImp x = prove1 (x1, y)
  | Just (T :> b, x1) <- pop isOImp x = prove1 (b >. x1, y)
  | Just (c :& d :> b, x1) <- pop isOImp x = prove1 (c :> (d :> b) >. x1, y)
  | Just (c :| d :> b, x1) <- pop isOImp x = prove1 (c :> b >. d :> b >. x1, y)
  | a :& b <- y = prove1 (x, a) && ((a /= b) && prove1 (x, b))
  | Just (a :| b, x1) <- pop isOr x = prove1 (a >. x1, y) && ((a /= b) && prove1 (b >. x1, y))
  | a :| b <- y, prove1 (x, a) || ((a /= b) && prove1 (x, b)) = True
  | Just ((c :> d) :> b, x1) <- pop isOImp x = prove1 (d :> b >. x1, c :> d) && prove1 (b >. x1, y)
  | otherwise = False
