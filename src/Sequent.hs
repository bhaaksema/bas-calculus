module Sequent where

import qualified Data.Either as E
import           Formula

-- | Sequent is a list of order and signed formula pairs
type Sequent = [(Int, SFormula)]

-- | Singleton right signed formula with order 0
singletonRight :: Formula -> Sequent
singletonRight a = [(0, Right a)]

-- | Get the right signed formulae
rights :: Sequent -> [Formula]
rights = E.rights . map snd

-- | Prepend a left signed formula with order 0
left :: Formula -> Sequent -> Sequent
left a = ((0, Left a) :)

-- | Prepend a right signed formula with order 0
right :: Formula -> Sequent -> Sequent
right a = ((0, Right a) :)

-- | Replace all right signed formulae with the given formula
setRight :: Formula -> Sequent -> Sequent
setRight a = ((0, Right a) :) . filter (E.isLeft . snd)

-- | Insert a signed formula into the sequent
insert :: (Int, SFormula) -> Sequent -> Sequent
insert a [] = [a]
insert a1 (a2 : as) = if fst a1 <= fst a2
  then a1 : a2 : as else a2 : insert a1 as

-- | Substitute a pair of order and signed formula
substi :: String -> Formula -> (Int, SFormula) -> (Int, SFormula)
substi s b (_, Left a)  | (True, c) <- alter (Just (s, b)) a = (0, Left c)
substi s b (_, Right a) | (True, c) <- alter (Just (s, b)) a = (0, Right c)
substi _ _ (p, a) = (p, a)
