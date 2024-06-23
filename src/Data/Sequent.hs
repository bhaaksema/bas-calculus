{-# LANGUAGE RecordWildCards #-}
module Data.Sequent (module Data.Sequent, C.Category (..)) where

import qualified Data.Collection as C
import           Data.Formula

data Sign = L | R deriving (Eq, Show)
data Sequent = S { getFresh :: Formula, left, right :: C.Collection }

-- | \(O(1)\). Sequent with singleton succedent.
fromFormula :: (Sign -> Formula -> C.Category) -> Formula -> Sequent
fromFormula sch formula = let
  sequent = S (succ formula) (C.empty $ sch L) (C.empty $ sch R)
  in add R (simplify formula) sequent

-- | \(O(n)\). Conversion to formula.
toFormula :: Sequent -> Formula
toFormula s = simplify $ foldr (:&) Top (C.elems $ left s)
  :> foldr (:|) Bot (C.elems $ right s)

-- | \(O(1)\). Generate a fresh variable.
fresh :: Sequent -> (Formula, Sequent)
fresh s | p <- succ (getFresh s) = (p, s { getFresh = p })

-- | \(O(m)\). Check if a formula is a member of the sequent.
member :: Sign -> Formula -> Sequent -> Bool
member L formula = C.member formula . left
member R formula = C.member formula . right

-- | \(O(1)\). Retrieve the formula with smallest category.
view :: Sequent -> Maybe (Sign, Formula, Sequent)
view s = case (C.view $ left s, C.view $ right s) of
  (Just (i, a, l), Just (j, b, r)) -- view is left-biased
    | i <= j    -> Just (L, a, s { left = l })
    | otherwise -> Just (R, b, s { right = r })
  (Just (_, a, l), _) -> Just (L, a, s { left = l })
  (_, Just (_, b, r)) -> Just (R, b, s { right = r })
  _ -> Nothing

-- | \(O(m)\). Add a formula to the sequent.
add :: Sign -> Formula -> Sequent -> Sequent
add L formula s = s { left = C.insert formula (left s) }
add R formula s = s { right = C.insert formula (right s) }

-- | \(O(1)\). Replace the right formulas, unlocking the left formulas.
setR :: Formula -> Sequent -> Sequent
setR formula = add R formula . delR

-- | \(O(1)\). Delete the right formulas, unlocking the left formulas.
delR :: Sequent -> Sequent
delR S { .. } = S { left = C.unlock left, right = C.empty (C.schedule right), .. }

-- | \(O(1)\). Check if the succedent is empty.
nullR :: Sequent -> Bool
nullR = null . C.elems . right

-- | \(O(1)\). Add a formula with maximum category.
lock :: Sign -> Formula -> Sequent -> Sequent
lock L formula s = s { left = C.lock formula (left s) }
lock R formula s = s { right = C.lock formula (right s) }

-- | \(O(n)\). Substitute sequent, may unlock formulas.
subst :: Bool -> Int -> Formula -> Sequent -> Sequent
subst t p f s = let subst' = C.subst $ substitute1 t (p, f)
  in s {left = subst' (left s), right = subst' (right s)}
