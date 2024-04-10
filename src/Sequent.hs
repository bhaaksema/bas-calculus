{-# LANGUAGE DeriveFunctor #-}
module Sequent where

import qualified Data.Set as S

import Formula

-- | Lock for formula
data Lock = L0 | L1 | L2 | L3 | L4 | LOCK
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Sign for formula
data Sign = L | R deriving (Eq, Show)

-- | Sequent is a pair of sets of formulas
type LabelFormula = (Lock, Formula)
data Pair a = S { left :: a, right :: a } deriving (Functor)
type Sequent = Pair (S.Set LabelFormula)

-- | \(O(1)\). Singleton formula set
singleton :: Formula -> Sequent
singleton f = S S.empty (S.singleton (minBound, f))

-- | \(O(\log n)\). Retrieve the formula with smallest lock
view :: Sequent -> (Sign, LabelFormula, Sequent)
view s = case (S.minView (left s), S.minView (right s)) of
  (Just (a@(i, _), l), Just (b@(j, _), r))
    | i <= j    -> (L, a, s { left = l })
    | otherwise -> (R, b, s { right = r })
  (Just (a, l), _) -> (L, a, s { left = l })
  (_, Just (b, r)) -> (R, b, s { right = r })
  _ -> (undefined, (maxBound, undefined), s)

-- | \(O(\log n)\). Add a left formula
addL :: Formula -> Sequent -> Sequent
addL f s = s { left = S.insert (minBound, f) (left s) }

-- | \(O(\log n)\). Add a right formula
addR :: Formula -> Sequent -> Sequent
addR f s = s { right = S.insert (minBound, f) (right s) }

-- | \(O(n)\). Replace the right formulas
setR :: Formula -> Sequent -> Sequent
setR f s = s { right = S.singleton (minBound, f) }

-- | \(O(n)\). Check if the there are no right formulas
nullR :: Sequent -> Bool
nullR s = S.null (right s)

-- | \(O(\log n)\). Insert a formula with next lock
next :: Sign -> LabelFormula -> Sequent -> Sequent
next L (i, f) s = s { left = S.insert (succ i, f) (left s) }
next R (i, f) s = s { right = S.insert (succ i, f) (right s) }

-- | \(O(\log n)\). Insert a formula with lock
lock :: Sign -> LabelFormula -> Sequent -> Sequent
lock L (_, f) s = s { left = S.insert (LOCK, f) (left s) }
lock R (_, f) s = s { right = S.insert (LOCK, f) (right s) }

-- | \(O(n)\). Unlock all formulas
unlock :: Sequent -> Sequent
unlock = fmap (S.map (\(_, f) -> (minBound, f)))

-- | \(O(n \log n)\). Substitute set, can reset lock
subst :: Bool -> Int -> Formula -> Sequent -> Sequent
subst t p f = fmap (S.map (\(i, a) ->
  let b = unitSubsti t (p, f) a
  in (if a == b then i else minBound, b)))
