{-# LANGUAGE DeriveFunctor #-}
module Sequent where

import           Control.Monad.State
import qualified Data.Set            as S

import Formula

-- | Priority for signed formula
data Prio = P0 | P1 | P2 | P3 | P4 | PMAX
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Sign for propositional formula
data Sign a = T a | F a
  deriving (Eq, Ord, Show, Functor)

-- | Sequent is a set of labelled formulae
type LabelFormula = (Prio, Sign Formula)
type Sequent = (S.Set LabelFormula)
type ProverState = (Formula, Sequent)

-- | \(O(1)\). Get a fresh variable
freshVar :: State ProverState Formula
freshVar = get >>= \(p, x) -> do
  let q = fresh p
  put (q, x) >> return q

-- | \(O(1)\). Get the sequent
getSet :: State ProverState Sequent
getSet = get >>= \(_, x) -> return x

-- | \(O(1)\). Set the sequent
putSet :: Sequent -> State ProverState ()
putSet x = get >>= \(p, _) -> put (p, x)

-- | \(O(1)\). State with one initial label formula
singletonF :: Formula -> ProverState
singletonF a = (a, S.singleton (minBound, F a))

-- | \(O(\log n)\). Retrieve the formula with smallest label
view :: State ProverState LabelFormula
view = getSet >>= \x -> case S.minView x of
  Just (f, y) -> putSet y >> return f
  _           -> return (maxBound, undefined)

-- | \(O(\log n)\). Insert a signed formula with smallest label
add :: Sign Formula -> State ProverState ()
add a = getSet >>= putSet . S.insert (minBound, a)

-- | \(O(\log n)\). Insert a formula with its label incremented
inc :: LabelFormula -> State ProverState ()
inc (i, a) = getSet >>= \x -> putSet $ S.insert (succ i, a) x

-- | \(O(n)\). Remove all F-signed formulae
delFs :: State ProverState ()
delFs = getSet >>= \x -> putSet (S.filter isT x)
  where isT (_, T _) = True; isT _ = False

-- | \(O(n \log n)\). Substitute sequent, can reset priority
mapSubsti :: Bool -> Int -> Formula -> State ProverState ()
mapSubsti t q c = getSet >>= \x ->
  putSet $ S.map (lmap $ unitSubsti t (q, c)) x where
  lmap f (i, a) | b <- f <$> a = (if a == b then i else minBound, b)
