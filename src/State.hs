module State where

import qualified Data.Set as S

import Formula

-- | Lock for formula
data Lock = L0 | L1 | L2 | L3 | L4 | LOCK
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Sign for formula
data Sign = L | R deriving (Eq, Show)

-- | FormulaSet is a set of signed formulae
type LabelFormula = (Lock, Formula)
type FormulaSet = (S.Set LabelFormula)
type Sequent = (FormulaSet, FormulaSet)

smap :: (FormulaSet -> FormulaSet) -> Sequent -> Sequent
smap f (x, y) = (f x, f y)

-- | \(O(1)\). Singleton formula set
singleton :: Formula -> Sequent
singleton f = (S.empty, S.singleton (minBound, f))

-- | \(O(\log n)\). Retrieve the formula with smallest lock
view :: Sequent -> (Sign, LabelFormula, Sequent)
view (x, y) = case (S.minView x, S.minView y) of
  (Just (a@(i, _), x1), Just (b@(j, _), y1))
    | i <= j    -> (L, a, (x1, y))
    | otherwise -> (R, b, (x, y1))
  (Just (a, x1), _) -> (L, a, (x1, y))
  (_, Just (b, y1)) -> (R, b, (x, y1))
  _ -> (undefined, (maxBound, undefined), (x, y))

-- | \(O(\log n)\). Add a left formula
addL :: Formula -> Sequent -> Sequent
addL f (x, y) = (S.insert (minBound, f) x, y)
infixr 5 `addL`

-- | \(O(\log n)\). Add a right formula
addR :: Formula -> Sequent -> Sequent
addR f (x, y) = (x, S.insert (minBound, f) y)
infixr 5 `addR`

-- | \(O(n)\). Replace the right formulas
setR :: Formula -> Sequent -> Sequent
setR f (x, _) = (x, S.singleton (minBound, f))

-- | \(O(n)\). Check if the there are no right formulas
nullR :: Sequent -> Bool
nullR (_, y) = S.null y

-- | \(O(\log n)\). Insert a formula with next lock
next :: Sign -> LabelFormula -> Sequent -> Sequent
next L (i, f) (x, y) = (S.insert (succ i, f) x, y)
next R (i, f) (x, y) = (x, S.insert (succ i, f) y)

-- | \(O(\log n)\). Insert a formula with lock
lock :: Sign -> LabelFormula -> Sequent -> Sequent
lock L (_, f) (x, y) = (S.insert (maxBound, f) x, y)
lock R (_, f) (x, y) = (x, S.insert (maxBound, f) y)

-- | \(O(n)\). Unlock all formulas
unlock :: Sequent -> Sequent
unlock = smap (S.map (\(_, f) -> (minBound, f)))

-- | \(O(n \log n)\). Substitute set, can reset lock
subst :: Bool -> Int -> Formula -> Sequent -> Sequent
subst t p f = smap (S.map (\(i, a) ->
  let b = unitSubsti t (p, f) a
  in (if a == b then i else minBound, b)))
