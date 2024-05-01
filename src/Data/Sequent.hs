{-# LANGUAGE DeriveFunctor #-}
module Data.Sequent (module Data.Sequent, C.Category (..)) where

import qualified Data.Collection as C
import           Data.Formula

data Sign = L | R deriving (Eq, Show)
data Pair a = S { getFresh :: Formula, left, right :: a }
  deriving (Show, Functor)
type Sequent = Pair C.Collection

-- | \(O(1)\). Sequent with singleton succedent.
fromFormula :: (Sign -> Formula -> C.Category) -> Formula -> Sequent
fromFormula sch f = add R (simplify f) (S (succ f) (C.empty $ sch L) (C.empty $ sch R))

-- | \(O(n)\). Conversion to formula.
toFormula :: Sequent -> Formula
toFormula s = simplify $ foldr (:&) Top (C.items $ left s)
  :> foldr (:|) Bot (C.items $ right s)

-- | \(O(1)\). Generate a fresh variable.
fresh :: Sequent -> (Formula, Sequent)
fresh s | p <- succ (getFresh s) = (p, s { getFresh = p })

-- | \(O(1)\). Retrieve the formula with smallest category.
view :: Sequent -> Maybe (Sign, Formula, Sequent)
view s = case (C.view $ left s, C.view $ right s) of
  (Just (i, a, l), Just (j, b, r)) -- view is left-biased
    | i <= j    -> Just (L, a, s { left = l })
    | otherwise -> Just (R, b, s { right = r })
  (Just (_, a, l), _) -> Just (L, a, s { left = l })
  (_, Just (_, b, r)) -> Just (R, b, s { right = r })
  _ -> Nothing

-- | \(O(1)\). Add a formula to the sequent.
add :: Sign -> Formula -> Sequent -> Sequent
add L f s = s { left = C.add f (left s) }
add R f s = s { right = C.add f (right s) }

-- | \(O(1)\). Replace the right formulas, unlocking the left formulas.
setR :: Formula -> Sequent -> Sequent
setR f = add R f . delR

-- | \(O(1)\). Delete the right formulas, unlocking the left formulas.
delR :: Sequent -> Sequent
delR s = s { left = C.unlock (left s), right = C.empty (C.sch $ right s) }

-- | \(O(1)\). Check if the succedent is empty.
nullR :: Sequent -> Bool
nullR = null . C.items . right

-- | \(O(1)\). Add a formula with maximum category.
lock :: Sign -> Formula -> Sequent -> Sequent
lock L f s = s { left = C.lock f (left s) }
lock R f s = s { right = C.lock f (right s) }

-- | \(O(n)\). Substitute sequent, may unlock formulas.
subst :: Bool -> Int -> Formula -> Sequent -> Sequent
subst t p f = fmap (C.map $ substitute1 t (p, f))
