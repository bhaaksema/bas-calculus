{-# LANGUAGE RecordWildCards #-}
module Data.Sequent (module Data.Sequent, C.Category (..)) where

import qualified Data.Collection as C
import           Data.Formula

data Sign = L | R deriving (Eq, Show)
data Sequent = Seq {
  getFresh    :: Formula,
  left, right :: C.Collection
}

-- | \(O(1)\). Sequent with singleton succedent.
fromFormula :: (Sign -> Formula -> C.Category) -> Formula -> Sequent
fromFormula schedule formula = let
  lhs = C.empty (schedule L)
  rhs = C.empty (schedule R)
  in add R formula (Seq (succ formula) lhs rhs)

-- | \(O(n)\). Conversion to formula.
toFormula :: Sequent -> Formula
toFormula Seq { .. } = foldr (:&) Top (C.elems left)
  :> foldr (:|) Bot (C.elems right)

-- | \(O(1)\). Generate a fresh variable.
fresh :: Sequent -> (Formula, Sequent)
fresh sequent = (variable, sequent { getFresh = variable })
  where variable = succ (getFresh sequent)

-- | \(O(m)\). Check if a formula is a member of the sequent.
member :: Sign -> Formula -> Sequent -> Bool
member L formula = C.member formula . left
member R formula = C.member formula . right

-- | \(O(1)\). Retrieve the formula with smallest category.
view :: Sequent -> Maybe (Sign, Formula, Sequent)
view Seq { .. } = case (C.view left, C.view right) of
  (Just (i, a, as), Just (j, b, bs)) -- view is left-biased
    | i <= j    -> Just (L, a, Seq { left = as, .. })
    | otherwise -> Just (R, b, Seq { right = bs, .. })
  (Just (_, a, as), _) -> Just (L, a, Seq { left = as, .. })
  (_, Just (_, b, bs)) -> Just (R, b, Seq { right = bs, .. })
  _ -> Nothing

-- | \(O(m)\). Add a formula to the sequent.
add :: Sign -> Formula -> Sequent -> Sequent
add L formula Seq { .. } = case formula of
  Top      -> Seq { .. }
  (a :& b) -> add L a $ add L b Seq { .. }
  _        -> Seq { left = C.insert formula left, .. }
add R formula Seq { .. } = case formula of
  Bot      -> Seq { .. }
  (a :| b) -> add R a $ add R b Seq { .. }
  _        -> Seq { right = C.insert formula right, .. }

-- | \(O(1)\). Replace the right formulas, unlocking the left formulas.
setR :: Formula -> Sequent -> Sequent
setR formula = add R formula . delR

-- | \(O(n)\). Delete the right formulas, unlocking the left formulas.
delR :: Sequent -> Sequent
delR Seq { .. } = Seq { left = C.unlock left, right = rhs, .. }
 where rhs = C.empty (C.schedule right)

-- | \(O(1)\). Check if the succedent is empty.
nullR :: Sequent -> Bool
nullR = null . C.elems . right

-- | \(O(1)\). Add a formula with maximum category.
lock :: Sign -> Formula -> Sequent -> Sequent
lock sign formula Seq { .. } = case sign of
  L -> Seq { left = C.lock formula left, .. }
  R -> Seq { right = C.lock formula right, .. }

-- | \(O(n)\). Substitute sequent, may unlock formulas.
subst :: Int -> Formula -> Sequent -> Sequent
subst variable formula Seq { .. } = let
  substitution = C.subst variable formula
  in Seq { left = substitution left, right = substitution right, .. }
