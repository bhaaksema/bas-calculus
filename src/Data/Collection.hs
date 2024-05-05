{-# LANGUAGE RecordWildCards #-}
module Data.Collection where

import Data.Formula

data Category = C0 | C1 | C2 | C3 | C4 | C5 | C6 | CX
  deriving (Eq, Ord, Show)

data Collection = C {
  sch :: Formula -> Category,
  c0, c1, c2, c3, c4, c5, c6, cx :: [Formula]
}

instance Show Collection where
  show C { .. } = show [c0, c1, c2, c3, c4, c5, c6, cx]

-- | \(O(1)\). Empty collection.
empty :: (Formula -> Category) -> Collection
empty sch = C sch [] [] [] [] [] [] [] []

-- | \(O(m)\). Add a formula according to the category.
add :: Formula -> Collection -> Collection
add f c = let
  ins x [] = [x]
  ins x (y : ys) = case compare x y of
    LT -> x : y : ys
    EQ -> x : ys
    GT -> y : ins x ys
  in case sch c f of
  C0 -> c { c0 = f `ins` c0 c }
  C1 -> c { c1 = f `ins` c1 c }
  C2 -> c { c2 = f `ins` c2 c }
  C3 -> c { c3 = f `ins` c3 c }
  C4 -> c { c4 = f `ins` c4 c }
  C5 -> c { c5 = f `ins` c5 c }
  C6 -> c { c6 = f `ins` c6 c }
  CX -> c { cx = f `ins` cx c }

-- | \(O(m)\). Check if a formula is a member of the collection.
member :: Formula -> Collection -> Bool
member f C { .. } = case sch f of
  C0 -> f `elem` c0
  C1 -> f `elem` c1
  C2 -> f `elem` c2
  C3 -> f `elem` c3
  C4 -> f `elem` c4
  C5 -> f `elem` c5
  C6 -> f `elem` c6
  CX -> f `elem` cx

-- | \(O(1)\). Retrieve all formulas uncategorized.
items :: Collection -> [Formula]
items C { .. } = c0 ++ c1 ++ c2 ++ c3 ++ c4 ++ c5 ++ c6 ++ cx

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
lock f c = c { cx = f : cx c }

-- | \(O(m)\). Unlock all locked formulas.
unlock :: Collection -> Collection
unlock c = foldr add (c { cx = [] }) (cx c)

-- | \(O(n)\). Apply a function to every formula in the collection.
map :: (Formula -> Formula) -> Collection -> Collection
map f C { .. } = foldr (\a -> let b = f a in if a == b then lock a else add b) c cx
  where c = foldr (add . f) (empty sch) (c0 ++ c1 ++ c2 ++ c3 ++ c4 ++ c5 ++ c6)
