module State where

import qualified Data.Set as S

import Formula

-- | Lock for formula
data Lock = L0 | L1 | L2 | L3 | L4 | LOCK
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Sign for formula
data Sign = T | F deriving (Eq, Ord, Show)

-- | FormulaSet is a set of signed formulae
type SignedFormula = (Lock, Sign, Formula)
type FormulaSet = (S.Set SignedFormula)

-- | \(O(1)\). Singleton formula set
singleton :: Formula -> FormulaSet
singleton f = S.singleton (minBound, F, f)

-- | \(O(\log n)\). Retrieve the formula with smallest lock
view :: FormulaSet -> (SignedFormula, FormulaSet)
view x = case S.minView x of
  Just fx -> fx
  _       -> ((maxBound, undefined, undefined), undefined)

-- | \(O(\log n)\). Add a T-signed formula
addT :: Formula -> FormulaSet -> FormulaSet
addT f = S.insert (minBound, T, f)
infixr 5 `addT`

-- | \(O(\log n)\). Add a F-signed formula
addF :: Formula -> FormulaSet -> FormulaSet
addF f = S.insert (minBound, F, f)
infixr 5 `addF`

-- | \(O(n)\). Replace the F-signed formulas
setF :: Formula -> FormulaSet -> FormulaSet
setF f = addF f . S.filter (\(_, s, _) -> s == T)

-- | \(O(n)\). Check if the there are no F-signed formulas
nullFs :: FormulaSet -> Bool
nullFs = S.null . S.filter (\(_, s, _) -> s == F)

-- | \(O(\log n)\). Insert a formula with next lock
next :: SignedFormula -> FormulaSet -> FormulaSet
next (i, s, f) = S.insert (succ i, s, f)

-- | \(O(\log n)\). Insert a formula with lock
lock :: SignedFormula -> FormulaSet -> FormulaSet
lock (_, f, s) = S.insert (maxBound, f, s)

-- | \(O(n)\). Unlock all formulas
unlock :: FormulaSet -> FormulaSet
unlock = S.map (\(_, s, a) -> (minBound, s, a))

-- | \(O(n \log n)\). Substitute set, can reset lock
subst :: Bool -> Int -> Formula -> FormulaSet -> FormulaSet
subst t p f = S.map (\(i, s, a) ->
  let b = unitSubsti t (p, f) a
  in (if a == b then i else minBound, s, b))
