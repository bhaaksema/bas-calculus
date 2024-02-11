{-# LANGUAGE DeriveFunctor #-}
module Sequent where

import qualified Data.Set as S

import Formula

-- | Priority for signed formula
data Prio = P0 | P1 | P2 | P3 | P4 | PMAX
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Sign for propositional formula
data Sign a = T a | F a
  deriving (Eq, Ord, Show, Functor)

-- | Sequent is a set of labelled formulae
type LabelFormula = (Prio, Sign Formula)
type Sequent = S.Set LabelFormula

-- | \(O(1)\). Sequent with one initial label formula
singleton :: Sign Formula -> Sequent
singleton a = S.singleton (minBound, a)

-- | \(O(\log n)\). Retrieve the formula with smallest label
view :: Sequent -> Maybe (LabelFormula, Sequent)
view x | Just v <- S.minView x, fst (fst v) < maxBound = Just v
view _ = Nothing

-- | \(O(\log n)\). Insert a signed formula with smallest label
(<|) :: Sign Formula -> Sequent -> Sequent
a <| x = S.insert (minBound, a) x
infixr <|

-- | \(O(\log n)\). Insert a formula with its label incremented
(<+) :: LabelFormula -> Sequent -> Sequent
(i, a) <+ x = S.insert (succ i, a) x
infixr <+

-- | \(O(n)\). Remove all F-signed formulae
delFs :: Sequent -> Sequent
delFs = S.filter isT where isT (_, T _) = True; isT _ = False

-- | \(O(n \log n)\). Substitute sequent, can reset priority
mapSubsti :: Bool -> String -> Formula -> Sequent -> Sequent
mapSubsti t p c = S.map (lmap (unitSubsti t (p, c))) where
  lmap f (i, a) | b <- f <$> a = (if a == b then i else minBound, b)
