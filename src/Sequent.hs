{-# LANGUAGE FlexibleInstances #-}
module Sequent where

import           Data.Bifunctor (bimap)
import qualified Data.Either    as E
import           Data.List      (uncons)
import qualified Data.Map       as M

import Formula

-- | Signed formula
type SFormula = Either Formula Formula

-- | Multi succedent sequent
data Sequent = S {
  smap :: M.Map String Formula,
  bin1 :: [SFormula],
  bin2 :: [SFormula]
}

-- | Create a sequent from a signed formula
singleton :: SFormula -> Sequent
singleton a = S M.empty [a] []

-- | Get the formulas in the antecedent
lefts :: Sequent -> [Formula]
lefts s = E.lefts (bin1 s ++ bin2 s)

-- | Get the formulas in the succedent
rights :: Sequent -> [Formula]
rights s = E.rights (bin1 s ++ bin2 s)

-- | Insert a signed formula into the sequent
-- resets the stack pointer
insert :: SFormula -> Sequent -> Sequent
insert a (S m as bs) = case bimap (subst m) (subst m) a of
  Left (Var str)        -> update str Top
  Left (Var str :> Bot) -> insert (Left Bot) $ update str Bot
  b                     -> S m (b : as) bs
  where update str b = as ++ bs +> S (M.insert str b m) [] []

-- | Insert a list of signed formulas into the sequent
-- resets the stack pointer
(+>) :: [SFormula] -> Sequent -> Sequent
(+>) as s = foldr insert s as
infix 4 +>

-- | Replace the succedent with a single formula
-- resets the stack pointer
setRight :: Formula -> Sequent -> Sequent
setRight a s = Right a : map Left (lefts s) +> S (smap s) [] []

-- | Pop the formula at the stack pointer
take :: Sequent -> Maybe (SFormula, Sequent)
take s = (\(a, as) -> (a, s {bin1 = as})) <$> uncons (bin1 s)

-- | Move the stack pointer, reset if empty
iterate :: Sequent -> (Bool, Sequent)
iterate (S m [] bs)       = (False, S m bs [])
iterate (S m (a : as) bs) = (True, S m as (a : bs))
