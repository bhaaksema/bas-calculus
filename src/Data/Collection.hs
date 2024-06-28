{-# LANGUAGE RecordWildCards #-}
module Data.Collection where

import Data.Formula

data Category = C0 | C1 | C2 | C3 | C4 | C5 | C6 | CX
  deriving (Eq, Ord)

data Collection = C {
  schedule :: Formula -> Category,
  -- This data structure avoids overhead of Map/Set.
  c0, c1, c2, c3, c4, c5, c6, cx :: [Formula]
}

-- | \(O(1)\). Empty collection.
empty :: (Formula -> Category) -> Collection
empty schedule = C schedule [] [] [] [] [] [] [] []

-- | \(O(1)\). Add a formula according to the category.
insert :: Formula -> Collection -> Collection
insert formula C { .. } = case schedule formula of
    C0 -> C { c0 = formula : c0, .. }
    C1 -> C { c1 = formula : c1, .. }
    C2 -> C { c2 = formula : c2, .. }
    C3 -> C { c3 = formula : c3, .. }
    C4 -> C { c4 = formula : c4, .. }
    C5 -> C { c5 = formula : c5, .. }
    C6 -> C { c6 = formula : c6, .. }
    CX -> C { cx = formula : cx, .. }

-- | \(O(m)\). Check if a formula is a member of the collection.
member :: Formula -> Collection -> Bool
member formula C { .. } = case schedule formula of
    C0 -> formula `elem` c0
    C1 -> formula `elem` c1
    C2 -> formula `elem` c2
    C3 -> formula `elem` c3
    C4 -> formula `elem` c4
    C5 -> formula `elem` c5
    C6 -> formula `elem` c6
    CX -> formula `elem` cx

-- | \(O(1)\). Retrieve all formulas uncategorized.
elems :: Collection -> [Formula]
elems C { .. } = c0 ++ c1 ++ c2 ++ c3 ++ c4 ++ c5 ++ c6 ++ cx

-- | \(O(1)\). Retrieve the formula with smallest category.
view :: Collection -> Maybe (Category, Formula, Collection)
view C { c0 = f : fs, .. } = Just (C0, f, C { c0 = fs, .. })
view C { c1 = f : fs, .. } = Just (C1, f, C { c1 = fs, .. })
view C { c2 = f : fs, .. } = Just (C2, f, C { c2 = fs, .. })
view C { c3 = f : fs, .. } = Just (C3, f, C { c3 = fs, .. })
view C { c4 = f : fs, .. } = Just (C4, f, C { c4 = fs, .. })
view C { c5 = f : fs, .. } = Just (C5, f, C { c5 = fs, .. })
view C { c6 = f : fs, .. } = Just (C6, f, C { c6 = fs, .. })
view _                     = Nothing

-- | \(O(1)\). Add a formula into the lock category.
lock :: Formula -> Collection -> Collection
lock formula collection = collection { cx = formula : cx collection }

-- | \(O(m)\). Unlock all locked formulas.
unlock :: Collection -> Collection
unlock collection = foldr insert (collection { cx = [] }) (cx collection)

-- | \(O(n)\). Substitute collection, may unlock formulas.
subst :: Int -> Formula -> Collection -> Collection
subst variable formula C { .. } = let
  substitution = substitute1 variable formula
  unlocked = c0 ++ c1 ++ c2 ++ c3 ++ c4 ++ c5 ++ c6
  collection = foldr (insert . substitution) (empty schedule) unlocked
  substLocked a = let b = substitution a in if a == b then lock a else insert b
  in foldr substLocked collection cx
