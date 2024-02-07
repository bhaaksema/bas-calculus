{-# LANGUAGE DeriveFunctor #-}
module Sequent where

import qualified Data.Set as S

import Formula (Formula (..), unitSubsti)

-- | Priority for signed formula
data Prio = P0 | P1 | P2 | P3 | P4 | P5
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Sign for propositional formula
data Sign a = T a | F a
  deriving (Eq, Ord, Show, Functor)

-- | Sequent is a set of prioritised signed formulae
type Sequent = S.Set (Prio, Sign Formula)

-- | \(O(\log n)\). Sequent with one signed formula
singleton :: Sign Formula -> Sequent
singleton a = a +> S.empty

-- | \(O(\log n)\). Retrieve the signed formula with first priority
view :: Sequent -> Maybe ((Prio, Sign Formula), Sequent)
view x | Just v <- S.minView x, fst (fst v) < maxBound = Just v
view _ = Nothing

-- | \(O(\log n)\). Insert a signed formula with initial priority
(+>) :: Sign Formula -> Sequent -> Sequent
a +> x = S.insert (minBound, a) x
infixr 4 +>

-- | \(O(\log n)\). Insert a signed formula with less priority
(<+) :: Sequent -> (Prio, Sign Formula) -> Sequent
x <+ (i, a) = S.insert (succ i, a) x
infixl 3 <+

-- | \(O(n)\). Check if the sequent contains no F-signed formulae
nullFs :: Sequent -> Bool
nullFs x = S.null $ S.filter isF x
  where isF (_, F _) = True; isF _ = False

-- | \(O(n)\). Replace all F-signed formulae with one given formula
replaceFs :: Formula -> Sequent -> Sequent
replaceFs a x = F a +> S.filter isT x
  where isT (_, T _) = True; isT _ = False

-- | \(O(n \log n)\). Substitute sequent, can reset priority
mapSubsti :: Bool -> String -> Formula -> Sequent -> Sequent
mapSubsti t p c = S.foldr (\(i, a) -> let b = unitSubsti t (p, c) <$> a
  in if invertible b then (b +>) else S.insert (i, b)) S.empty where
  invertible (F (_ :> _)) = False
  invertible (T (_ :> _)) = False
  invertible _            = True
