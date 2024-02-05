{-# LANGUAGE DeriveFunctor #-}
module Sequent where

import qualified Data.Set as S

import Formula (Formula (..), alter1)

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
(+>) a = case a of
  T Bot            -> const (S.singleton (0, a))
  F Top            -> const (S.singleton (0, a))
  T Top            -> id
  F Bot            -> id
  T (Var _)        -> S.insert (1, a)
  F (Var _)        -> (<+ a)
  T (Var _ :> Bot) -> S.insert (1, a)
  T (b :& c)       -> (T b +>) . (T c +>)
  F (b :| c)       -> (F b +>) . (F c +>)
  F (_ :> _)       -> S.insert (2, a)
  T (_ :| _)       -> S.insert (3, a)
  F (_ :& _)       -> S.insert (3, a)
  T (_ :> _)       -> S.insert (4, a)
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

-- | \(O(n \log n)\). Substitute sequent, relabels formulae when changed
substi :: String -> Formula -> Sequent -> Sequent
substi p c = S.foldr (\(i, a) -> let b = alter1 p c <$> a
  in (if a == b then S.insert (i, a) else (b +>))) S.empty
