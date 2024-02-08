{-# LANGUAGE DeriveFunctor, LambdaCase #-}
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
type PFormula = (Prio, Sign Formula)
type Sequent = (S.Set PFormula, [PFormula])

-- | \(O(\log n)\). Sequent with one signed formula
singleton :: Sign Formula -> Sequent
singleton a = (S.empty, [(P0, a)])

-- | \(O(\log n)\). Retrieve the signed formula with first priority
view :: Sequent -> Maybe (PFormula, Sequent)
view (x, a : as) = Just (a, (x, as))
view (x, []) | Just (a, y) <- S.minView x, fst a < maxBound = Just (a, (y, []))
view _ = Nothing

-- | \(O(\log n)\). Insert a signed formula with initial priority
(+>) :: Sign Formula -> Sequent -> Sequent
a +> (x, as) = (x, (P0, a) : as)
infixr 4 +>

-- | \(O(\log n)\). Insert a signed formula with some priority
(<+) :: Sequent -> PFormula -> Sequent
(x, es) <+ a = (S.insert a x, es)
infixl 3 <+

-- | \(O(n)\). Check if the sequent contains no F-signed formulae
nullFs :: Sequent -> Bool
nullFs (x, _) = S.null $ S.filter (\case (_, F _) -> True; _ -> False) x

-- | \(O(n)\). Remove all F-signed formulae
delFs :: Sequent -> Sequent
delFs (x, as) = (S.filter (\case (_, T _) -> True; _ -> False) x, as)

-- | \(O(n \log n)\). Substitute sequent, can reset priority
mapSubsti :: Bool -> String -> Formula -> Sequent -> Sequent
mapSubsti t p c (x, as) = S.foldr (\(i, a) -> let b = unitSubsti t (p, c) <$> a
  in if invertible b then (b +>) else (<+ (i, b))) (S.empty, as) x where
  invertible (F (_ :> _)) = False
  invertible (T (_ :> _)) = False
  invertible _            = True
