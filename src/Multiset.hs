module Multiset where

import           Data.Bifunctor (bimap)
import qualified Data.Either    as E
import           Data.List      (uncons)
import qualified Data.Map       as M

import Formula

-- | Multiset of signed formulas
data Multiset = M {
  bin1 :: [SFormula],
  bin2 :: [SFormula]
}

-- | Create a sequent from a signed formula
singleton :: SFormula -> Multiset
singleton a = M [a] []

-- | Get the formulas in the antecedent
lefts :: Multiset -> [Formula]
lefts s = E.lefts (bin1 s ++ bin2 s)

-- | Get the formulas in the succedent
rights :: Multiset -> [Formula]
rights s = E.rights (bin1 s ++ bin2 s)

-- | Insert a signed formula into the sequent
-- resets the stack pointer
insert :: SFormula -> Multiset -> Multiset
insert a (M as bs) = M (a : as ++ bs) []

-- | Insert a list of signed formulas into the sequent
-- resets the stack pointer
(+>) :: [SFormula] -> Multiset -> Multiset
(+>) as s = foldr insert s as
infix 4 +>

-- | Replace the succedent with a single formula
-- resets the stack pointer
setRight :: Formula -> Multiset -> Multiset
setRight a s = Right a : map Left (lefts s) +> M [] []

-- | Pop the formula at the stack pointer
take :: Multiset -> Maybe (SFormula, Multiset)
take s = (\(a, as) -> (a, s {bin1 = as})) <$> uncons (bin1 s)

-- | Move the stack pointer, reset if empty
iterate :: Multiset -> Multiset
iterate (M [] bs)       = M bs []
iterate (M (a : as) bs) = M as (a : bs)

-- | Apply a substitution to a sequent
-- resets the stack pointer
fmap :: String -> Formula -> Multiset -> Multiset
fmap str a (M as bs) = M (map (bimap f f) (as ++ bs)) []
  where f = subst (M.singleton str a)
