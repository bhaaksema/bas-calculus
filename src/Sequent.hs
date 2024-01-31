module Sequent where

import qualified Data.List as L

import Formula

-- | Sequent is a priority queue of signed formulae
type SequentItem = (Word, Sign Formula)
type Sequent = [SequentItem]

-- | Prepend a signed formula with initial priority
(<|) :: Sign Formula -> Sequent -> Sequent
(<|) a = ((0, a) :)
infixr <|

-- | Check if the sequent contains no F-signed formulae
nullFs :: Sequent -> Bool
nullFs x = null [a | (_, F a) <- x]

-- | Replace all F-signed formulae with one given formula
replaceFs :: Formula -> Sequent -> Sequent
replaceFs a x = F a <| [b | b@(_, T _) <- x]

-- | Enqueue a signed formula into the sequent
insert :: SequentItem -> Sequent -> Sequent
insert = L.insertBy (\a b -> compare (fst a) (fst b))

-- | Substitute Sequent, reset formula priority when changed
substi :: String -> Formula -> Sequent -> Sequent
substi s c = substi' ([], []) where
  substi' (x, y) [] = x ++ y
  substi' (x, y) ((p, a) : z) | b <- alter1 s c <$> a =
    substi' (if b /= a then (b <| x, y) else (x, (p, b) : y)) z
