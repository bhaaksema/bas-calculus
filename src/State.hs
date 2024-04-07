module State where

import           Control.Monad.State
import qualified Data.Set            as S

import Formula

-- | Label for formula
data Label = L0 | L1 | L2 | L3 | LOCK
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Sign for formula
data Sign = T | F deriving (Eq, Ord, Show)

-- | FormulaSet is a set of signed formulae
type SignedFormula = (Label, Sign, Formula)
type FormulaSet = (S.Set SignedFormula)
type ProverState = (Formula, FormulaSet)

-- | \(O(1)\). Get a fresh variable
freshV :: State ProverState Formula
freshV = get >>= \(p, x) -> do
  let q = fresh p
  put (q, x) >> return q

-- | \(O(1)\). Modify the set
modifySet :: (FormulaSet -> FormulaSet) -> State ProverState ()
modifySet f = gets (f . snd) >>= \x -> putSet x

-- | \(O(1)\). Replace the set
putSet :: FormulaSet -> State ProverState ()
putSet x = gets fst >>= \p -> put (p, x)

-- | \(O(1)\). State with one initial label formula
newState :: Formula -> ProverState
newState f = (f, S.singleton (minBound, F, f))

-- | \(O(\log n)\). Retrieve the formula with smallest label
view :: State ProverState SignedFormula
view = gets snd >>= \x -> case S.minView x of
  Just (f, y) -> putSet y >> return f
  _           -> return (maxBound, undefined, undefined)

-- | \(O(\log n)\). Insert a signed formula with smallest label
add :: SignedFormula -> State ProverState ()
add h = modifySet (S.insert h)

-- | \(O(\log n)\). Add a T-signed formula
addT :: Formula -> State ProverState ()
addT f = add (minBound, T, f)

-- | \(O(\log n)\). Add a F-signed formula
addF :: Formula -> State ProverState ()
addF f = add (minBound, F, f)

-- | \(O(\log n)\). Insert a formula with incremented label
next :: SignedFormula -> State ProverState ()
next (i, s, f) = add (succ i, s, f)

-- | \(O(\log n)\). Insert a formula with locked label
lock :: SignedFormula -> State ProverState ()
lock (_, f, s) = add (maxBound, f, s)

-- | \(O(n)\). Replace the F-signed formulae
setF :: Formula -> State ProverState ()
setF f = modifySet (S.filter isT) >> addF f
  where isT = (/= F) . (\(_, s, _) -> s)

-- | \(O(n \log n)\). Substitute set, can reset label
subst :: Bool -> Int -> Formula -> State ProverState ()
subst t p f = modifySet (S.map (\(i, s, a) ->
  let b = unitSubsti t (p, f) a
  in (if a == b then i else minBound, s, b)))

-- Temporary solution for implication
reset :: State ProverState ()
reset = modifySet (S.map (\(_, s, a) -> (minBound, s, a)))

countFs :: State ProverState Int
countFs = gets ((S.size . S.filter (\(_, s, _) -> s == F)) . snd)
