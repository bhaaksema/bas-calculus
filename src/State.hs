module State where

import           Control.Monad.State
import qualified Data.Set            as S

import Formula

-- | Lock for formula
data Lock = L0 | L1 | L2 | L3 | L4 | LOCK
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Sign for formula
data Sign = T | F deriving (Eq, Ord, Show)

-- | FormulaSet is a set of signed formulae
type SignedFormula = (Lock, Sign, Formula)
type FormulaSet = (S.Set SignedFormula)
type ProverState = (Formula, FormulaSet)

-- | \(O(1)\). Get a fresh variable
freshV :: State ProverState Formula
freshV = get >>= \(p, x) -> do
  let q = fresh p
  put (q, x) >> return q

-- | \(O(f)\). Modify the set
modifySet :: (FormulaSet -> FormulaSet) -> State ProverState ()
modifySet f = gets (f . snd) >>= \x -> putSet x

-- | \(O(1)\). Replace the set
putSet :: FormulaSet -> State ProverState ()
putSet x = gets fst >>= \p -> put (p, x)

-- | \(O(1)\). State with one initial signed formula
newState :: Formula -> ProverState
newState f = (f, S.singleton (minBound, F, f))

-- | \(O(\log n)\). Retrieve the formula with smallest lock
view :: State ProverState SignedFormula
view = gets snd >>= \x -> case S.minView x of
  Just (f, y) -> putSet y >> return f
  _           -> return (maxBound, undefined, undefined)

-- | \(O(\log n)\). Insert a signed formula with smallest lock
add :: SignedFormula -> State ProverState ()
add h = modifySet (S.insert h)

-- | \(O(\log n)\). Add a T-signed formula
addT :: Formula -> State ProverState ()
addT f = add (minBound, T, f)

-- | \(O(\log n)\). Add a F-signed formula
addF :: Formula -> State ProverState ()
addF f = add (minBound, F, f)

-- | \(O(\log n)\). Insert a formula with next lock
next :: SignedFormula -> State ProverState ()
next (i, s, f) = add (succ i, s, f)

-- | \(O(\log n)\). Insert a formula with lock
lock :: SignedFormula -> State ProverState ()
lock (_, f, s) = add (maxBound, f, s)

-- | \(O(n)\). Replace the F-signed formulae
setF :: Formula -> State ProverState ()
setF f = modifySet (S.filter isT) >> addF f
  where isT = (/= F) . (\(_, s, _) -> s)

-- | \(O(n \log n)\). Substitute set, can reset lock
subst :: Bool -> Int -> Formula -> State ProverState ()
subst t p f = modifySet (S.map (\(i, s, a) ->
  let b = unitSubsti t (p, f) a
  in (if a == b then i else minBound, s, b)))

-- | \(O(n)\). Unlock all formulas
unlock :: State ProverState ()
unlock = modifySet (S.map (\(_, s, a) -> (minBound, s, a)))

-- | \(O(n)\). Check if the there are no F-signed formulas
nullFs :: State ProverState Bool
nullFs = gets ((S.null . S.filter (\(_, s, _) -> s == F)) . snd)
