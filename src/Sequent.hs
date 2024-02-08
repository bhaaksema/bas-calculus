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

-- | Sequent is a set of prioritised signed formulae
type Sequent = S.Set (Prio, Sign Formula)

-- | \(O(1)\). Empty sequent
empty :: Sequent
empty = S.empty

-- | \(O(\log n)\). Insert a signed formula
(<|) :: (Prio, Sign Formula) -> Sequent -> Sequent
(<|) = S.insert
infixr 4 <|

-- | \(O(\log n)\). Retrieve the signed formula with first priority
view :: Sequent -> Maybe (Sign Formula, Sequent)
view x | Just ((i, a), y) <- S.minView x, i < maxBound = Just (a, y)
view _ = Nothing

-- | \(O(n)\). Check if the sequent contains no F-signed formulae
nullFs :: Sequent -> Bool
nullFs x = S.null $ S.filter isF x
  where isF (_, F _) = True; isF _ = False

-- | \(O(n)\). Remove all F-signed formulae
delFs :: Sequent -> Sequent
delFs = S.filter isT
  where isT (_, T _) = True; isT _ = False

-- | \(O(n \log n)\). Substitute sequent, can reset priority
mapSubsti :: (Sign Formula -> Sequent -> Sequent)
  -> Bool -> String -> Formula -> Sequent -> Sequent
mapSubsti pinsert t p c = S.foldr (\(i, a) -> let b = f a in
  if a /= b then pinsert b else ((i, b) <|)) S.empty
  where f = (unitSubsti t (p, c) <$>)
