module Sequent where

import qualified Data.Map as M

import Formula
import Multiset

-- | Multi succedent sequent
data Sequent = Sequent {
  smap  :: M.Map String Formula,
  left  :: Multiset,
  right :: Multiset
}

-- | Create a sequent from a formula
singletonRight :: Formula -> Sequent
singletonRight a = Sequent M.empty empty (singleton $ simplify a)

-- | Insert a formula into the antecedent
(+<) :: Formula -> Sequent -> Sequent
a +< s = case a of
  Var str        -> apply str Top
  Var str :> Bot -> apply str Bot
  _              -> s {left = insert (subst (smap s) a) (left s)}
  where
    apply str atom = let -- side effect, resets stack pointers
      s1 = Sequent (M.insert str atom (smap s)) (singleton atom) empty
      s2 = foldr ((+<) . subst (smap s1)) s1 (toList (left s))
      in foldr ((+>) . subst (smap s2)) s2 (toList (right s))
infixr 5 +<

-- | Insert a formula into the succedent
(+>) :: Formula -> Sequent -> Sequent
a +> s = s {right = insert (subst (smap s) a) (right s)}
infixr 5 +>

-- | Take the first formula from the antecedent
takeLeft :: Sequent -> Maybe (Formula, Sequent)
takeLeft s = (\(a, x1) -> (a, s {left = x1})) <$> pop (left s)

-- | Take the first formula from the succedent
takeRight :: Sequent -> Maybe (Formula, Sequent)
takeRight s = (\(a, y1) -> (a, s {right = y1})) <$> pop (right s)

-- | Move a new formula to the front
iterate :: Sequent -> Maybe Sequent
iterate s
  | Just l <- down (left s) = Just s {left = l}
  | Just r <- down (right s) = Just s {right = r}
  | otherwise = Nothing

-- | Reset the order of the formulas
reset :: Sequent -> Sequent
reset s = s {left = ceil (left s), right = ceil (right s)}
