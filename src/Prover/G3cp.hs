module Prover.G3cp where

import Formula  (Formula)
import MultiSet

type Sequent = (MultiSet, MultiSet)

-- Prove a formula
prove :: Formula -> Bool
prove a = prove1 (empty, singleton a)

-- Prove a sequent
prove1 :: Sequent -> Bool
prove1 s = axiom s || rule s

-- Check if the sequent satisfies an axiom
axiom :: Sequent -> Bool
axiom (x, y) = shareVar x y || unF x || unT y

-- Check if the sequent satisfies a rule
rule :: Sequent -> Bool
rule (x, y)
  | Just (a, b, x1) <- popAn x = prove1 (a >. b >. x1, y)
  | Just (a, b, y1) <- popOr y = prove1 (x, a >. b >. y1)
  | Just (a, b, y1) <- popIm y = prove1 (a >. x, b >. y1)
  | Just (a, b, y1) <- popAn y = prove1 (x, a >. y1) && prove1 (x, b >. y1)
  | Just (a, b, x1) <- popOr x = prove1 (a >. x1, y) && prove1 (b >. x1, y)
  | Just (a, b, x1) <- popIm x = prove1 (x1, a >. y) && prove1 (b >. x1, y)
  | otherwise = False
