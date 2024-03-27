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

-- | Fresh instance for Sign
instance Fresh a => Fresh (Sign a) where
  fresh (T a) = fresh a
  fresh (F a) = fresh a

-- | Sequent is a set of labelled formulae
type LabelFormula = (Prio, Sign Formula)
type Sequent = (Formula, S.Set LabelFormula)

-- | Fresh variable from Sequent
freshVar :: Sequent -> (Formula, Sequent)
freshVar (p, x) = (p, (fresh p, x))

-- | \(O(1)\). Sequent with one initial label formula
singleton :: Sign Formula -> Sequent
singleton a = (fresh a, S.singleton (minBound, a))

-- | \(O(\log n)\). Retrieve the formula with smallest label
view :: Sequent -> Maybe (LabelFormula, Sequent)
view (p, x) | Just (a, y) <- S.minView x, fst a < maxBound = Just (a, (p, y))
view _ = Nothing

-- | \(O(\log n)\). Insert a signed formula with smallest label
(<|) :: Sign Formula -> Sequent -> Sequent
a <| (p, x) = (p, S.insert (minBound, a) x)
infixr <|

-- | \(O(\log n)\). Insert a formula with its label incremented
(<+) :: LabelFormula -> Sequent -> Sequent
(i, a) <+ (p, x) = (p, S.insert (succ i, a) x)
infixr <+

-- | \(O(n)\). Remove all F-signed formulae
delFs :: Sequent -> Sequent
delFs (p, x) = (p, S.filter isT x) where isT (_, T _) = True; isT _ = False

-- | \(O(n \log n)\). Substitute sequent, can reset priority
mapSubsti :: Bool -> Int -> Formula -> Sequent -> Sequent
mapSubsti t q c (p, x) = (p, S.map (lmap $ unitSubsti t (q, c)) x) where
  lmap f (i, a) | b <- f <$> a = (if a == b then i else minBound, b)
