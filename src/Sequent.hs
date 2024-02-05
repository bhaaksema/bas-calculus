{-# LANGUAGE DeriveFunctor #-}
module Sequent where

import qualified Data.Set as S

import Formula (Formula (..), unitSubsti)

-- | Sign for propositional formula
data Sign a = T a | F a
  deriving (Eq, Ord, Show, Functor)

-- | Sequent is a set of labelled signed formulae
type Sequent = S.Set (Int, Sign Formula)

-- | Label for dormant signed formulae
sleep :: Int
sleep = maxBound :: Int

-- | \(O(\log n)\). Sequent with one labelled signed formula
singleton :: Sign Formula -> Sequent
singleton a = a +> S.empty

-- | \(O(\log n)\). Inspect the sequent
view :: Sequent -> Maybe (Sign Formula, Sequent)
view x | Just ((i, a), y) <- S.minView x, i < sleep = Just (a, y)
view _ = Nothing

-- | \(O(\log n)\). Insert a signed formula with label
(+>) :: Sign Formula -> Sequent -> Sequent
T Top +> x = x
F Bot +> x = x
T (a :& b) +> x = T a +> T b +> x
F (a :| b) +> x = F a +> F b +> x
a +> x | i <- case a of
  T Bot            -> 0
  F Top            -> 0
  T (Var _)        -> 1
  F (Var _)        -> 1
  T (Var _ :> Bot) -> 1
  F (_ :> _)       -> 2
  T (_ :| _)       -> 3
  F (_ :& _)       -> 3
  T (_ :> _)       -> 4
  = S.insert (i, a) x
infixr +>

-- | \(O(\log n)\). Insert a signed formula as dormant
(<+) :: Sequent -> Sign Formula -> Sequent
x <+ a = S.insert (sleep, a) x
infixl <+

-- | \(O(n)\). Check if the sequent contains no F-signed formulae
nullFs :: Sequent -> Bool
nullFs x = S.null $ S.filter isF x
  where isF (_, F _) = True; isF _ = False

-- | \(O(n)\). Replace all F-signed formulae with one given formula
replaceFs :: Formula -> Sequent -> Sequent
replaceFs a x = F a +> S.filter isT x
  where isT (_, T _) = True; isT _ = False

-- | \(O(n \log n)\). Substitute sequent, wakes up formulae
mapSubsti :: Bool -> String -> Formula -> Sequent -> Sequent
mapSubsti t p c = S.foldr (\(i, a) -> let b = unitSubsti t (p, c) <$> a in
  S.insert (if b `elem` [T Bot, F Top] then 0 else i, b)) S.empty
