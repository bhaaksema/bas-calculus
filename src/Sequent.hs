{-# LANGUAGE DeriveFunctor #-}
module Sequent where

import qualified Data.Set as S

import Formula (Formula (..), unitSubsti)

-- | Sign for propositional formula
data Sign a = T a | F a
  deriving (Eq, Ord, Show, Functor)

-- | Priority for signed formula
data Prio = P0 | P1 | P2 | P3 | P4 | P5
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | View for sequent
data View a = Fresh a | Stale a | Empty

-- | Sequent is a set of prioritised signed formulae
type SFormula = Sign Formula
type Sequent = ([SFormula], S.Set (Prio, SFormula))

-- | \(O(\log n)\). Sequent with one signed formula
singleton :: SFormula -> Sequent
singleton a = ([a], S.empty)

-- | \(O(\log n)\). Retrieve the signed formula with first priority
view :: Sequent -> View (SFormula, Sequent)
view (a : as, x) = Fresh (a, (as, x))
view (_, x) | Just ((i, a), y) <- S.minView x, i < maxBound = Stale (a, ([], y))
view _ = Empty

-- | \(O(\log n)\). Insert a fresh signed formula
(+>) :: SFormula -> Sequent -> Sequent
a +> (as, x) = (a : as, x)
infixr 4 +>

-- | \(O(\log n)\). Insert a scheduled formula
(|>) :: (Prio, SFormula) -> Sequent -> Sequent
a |> (as, x) = (as, S.insert a x)
infixr 4 |>

-- | \(O(n)\). Check if the sequent contains no F-signed formulae
nullFs :: Sequent -> Bool
nullFs (_, x) = S.null $ S.filter isF x
  where isF (_, F _) = True; isF _ = False

-- | \(O(n)\). Remove all F-signed formulae
delFs :: Sequent -> Sequent
delFs (as, x) = (as, S.filter isT x)
  where isT (_, T _) = True; isT _ = False

-- | \(O(n \log n)\). Substitute sequent, can reset priority
mapSubsti :: Bool -> String -> Formula -> Sequent -> Sequent
mapSubsti t p c (as, x) = S.foldr (\(i, a) -> let b = f a in
  (if a /= b then (b +>) else ((i, b) |>))) (map f as, S.empty) x
  where f = (unitSubsti t (p, c) <$>)
