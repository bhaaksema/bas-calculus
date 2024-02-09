{-# LANGUAGE DeriveFunctor #-}
module Sequent where

import qualified Data.Set as S

import Formula

-- | Priority for signed formula
data Prio = P0 | P1 | P2 | P3 | P4 | P9
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Sign for propositional formula
data Sign a = T a | F a
  deriving (Eq, Ord, Show, Functor)

-- | Sequent is a set of prioritised signed formulae
type Sequent = S.Set (Prio, Sign Formula)

-- | \(O(\log n)\). Sequent with one signed formula
singleton :: Sign Formula -> Sequent
singleton a = a <| S.empty

-- | \(O(\log n)\). Retrieve the signed formula with first priority
view :: Sequent -> Maybe ((Prio, Sign Formula), Sequent)
view x | Just v <- S.minView x, fst (fst v) < maxBound = Just v
view _ = Nothing

-- | \(O(\log n)\). Insert a signed formula with initial priority
(<|) :: Sign Formula -> Sequent -> Sequent
a <| x = S.insert (minBound, a) x
infixr <|

-- | \(O(\log n)\). Insert a signed formula with next priority
(<+) :: (Prio, Sign Formula) -> Sequent -> Sequent
(i, a) <+ x = S.insert (succ i, a) x
infixr <+

-- | \(O(n)\). Remove all F-signed formulae
delFs :: Sequent -> Sequent
delFs = S.filter isT
  where isT (_, T _) = True; isT _ = False

-- | \(O(n \log n)\). Substitute sequent, can reset priority
mapSubsti :: Bool -> String -> Formula -> Sequent -> Sequent
mapSubsti t p c = S.foldr (\(i, a) -> let b = unitSubsti t (p, c) <$> a
  in if a /= b then (b <|) else S.insert (i, b)) S.empty
