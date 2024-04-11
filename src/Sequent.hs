{-# LANGUAGE DeriveFunctor #-}
module Sequent where

import qualified Data.Set as S

import Formula

-- | Lock for formula
data Lock = INIT | L1 | L2 | LOCK
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Sign for formula
data Sign = L | R deriving (Eq, Show)

-- | Sequent is a pair of formula sets
data Pair a = S { left :: a, right :: a } deriving (Functor)
type Sequent = Pair (S.Set (Lock, Formula))

-- | \(O(1)\). Singleton formula set
singleton :: Formula -> Sequent
singleton f = S S.empty (S.singleton (minBound, f))

-- | \(O(\log n)\). Retrieve the formula with smallest lock
view :: Sequent -> (Lock, (Sign, Formula), Sequent)
view s = case (S.minView (left s), S.minView (right s)) of
  (Just ((i, a), l), Just ((j, b), r))
    | i <= j    -> (i, (L, a), s { left = l })
    | otherwise -> (j, (R, b), s { right = r })
  (Just ((i, a), l), _) -> (i, (L, a), s { left = l })
  (_, Just ((j, b), r)) -> (j, (R, b), s { right = r })
  _ -> (LOCK, (undefined, undefined), s)

-- | \(O(\log n)\). Add a left formula
addL :: Formula -> Sequent -> Sequent
addL f s = s { left = S.insert (minBound, f) (left s) }

-- | \(O(\log n)\). Add a right formula
addR :: Formula -> Sequent -> Sequent
addR f s = s { right = S.insert (minBound, f) (right s) }

-- | \(O(1)\). Replace the right formulas
setR :: Formula -> Sequent -> Sequent
setR f s = s { right = S.singleton (minBound, f) }

-- | \(O(1)\). Delete the right formulas
delR :: Sequent -> Sequent
delR s = s { right = S.singleton (minBound, Bot) }

-- | \(O(1)\). Check if the there are no right formulas
nullR :: Sequent -> Bool
nullR s = S.null (right s)

-- | \(O(\log n)\). Insert a formula with next lock
next :: (Lock, Sign, Formula) -> Sequent -> Sequent
next (i, L, f) s = s { left = S.insert (succ i, f) (left s) }
next (i, R, f) s = s { right = S.insert (succ i, f) (right s) }

-- | \(O(\log n)\). Insert a formula with some lock
push :: Lock -> (Sign, Formula) -> Sequent -> Sequent
push i (L, f) s = s { left = S.insert (i, f) (left s) }
push i (R, f) s = s { right = S.insert (i, f) (right s) }

-- | \(O(\log n)\). Insert a formula with max lock
lock :: (Sign, Formula) -> Sequent -> Sequent
lock (L, f) s = s { left = S.insert (LOCK, f) (left s) }
lock (R, f) s = s { right = S.insert (LOCK, f) (right s) }

-- | \(O(n)\). Unlock all formulas
unlock :: Sequent -> Sequent
unlock = fmap (S.map (\(_, f) -> (minBound, f)))

-- | \(O(n \log n)\). Substitute set, can reset lock
subst :: Bool -> Int -> Formula -> Sequent -> Sequent
subst t p f = fmap (S.map (\(i, a) ->
  let b = unitSubsti t (p, f) a
  in if a == b then (i, a) else (minBound, b)))
