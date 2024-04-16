{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}
module Data.Sequent where

import qualified Data.Collection as C
import           Data.Formula

data Pair a = S { left, right :: a } deriving (Show, Functor)
type Sequent = Pair C.Collection

-- | \(O(1)\). Empty sequent.
empty :: (Formula -> C.Category) -> (Formula -> C.Category) -> Sequent
empty schL schR = S (C.empty schL) (C.empty schR)

-- | \(O(1)\). Sequent with one right formula.
singletonR :: (Formula -> C.Category) -> (Formula -> C.Category) -> Formula -> Sequent
singletonR schL schR f = addR f $ empty schL schR

-- | \(O(1)\). Retrieve the formula with smallest category.
view :: Sequent -> Maybe (Either (Formula, Sequent) (Formula, Sequent))
view s = case (C.view $ left s, C.view $ right s) of
  (Just (i, a, l), Just (j, b, r))
    | i <= j    -> Just (Left (a, s { left = l }))
    | otherwise -> Just (Right (b, s { right = r }))
  (Just (_, a, l), _) -> Just (Left (a, s { left = l }))
  (_, Just (_, b, r)) -> Just (Right (b, s { right = r }))
  _ -> Nothing

-- | \(O(1)\). Add a formula to the left.
addL :: Formula -> Sequent -> Sequent
addL f s = s { left = C.add f (left s) }

-- | \(O(1)\). Add a formula to the right.
addR :: Formula -> Sequent -> Sequent
addR f s = s { right = C.add f (right s) }

-- | \(O(1)\). Replace the Collection.
setR :: Formula -> Sequent -> Sequent
setR f s = s { right = C.add f (C.empty $ C.sch $ right s) }

-- | \(O(1)\). Delete the right formulas.
delR :: Sequent -> Sequent
delR s = s { right = C.empty $ C.sch $ right s }

-- | \(O(1)\). Check if the succedent is empty.
nullR :: Sequent -> Bool
nullR = null . C.items . right

-- | \(O(1)\). Add a formula left with maximum category.
lockL :: Formula -> Sequent -> Sequent
lockL f s = s { left = C.lock f (left s) }

-- | \(O(1)\). Add a formula right with maximum category.
lockR :: Formula -> Sequent -> Sequent
lockR f s = s { right = C.lock f (right s) }

-- | \(O(n)\). Unlock all formulas.
unlock :: Sequent -> Sequent
unlock = fmap C.unlock

-- | \(O(n)\). Substitute sequent.
subst :: Bool -> Int -> Formula -> Sequent -> Sequent
subst t p f = fmap $ C.map $ unitSubsti t (p, f)

-- | \(O(n)\). Conversion to formula.
toFormula :: Sequent -> Formula
toFormula s = simply $ foldr (:&) Top (C.items $ left s)
  :> foldr (:|) Bot (C.items $ right s)
