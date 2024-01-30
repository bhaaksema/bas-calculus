module Sequent where

import Data.List (insertBy)
import Formula

-- | Sequent is a queue of formulae
type SequentItem = (Word, Sign Formula)
type Sequent = [SequentItem]

-- | Prepend a signed formula with order 0
(<|) :: Sign Formula -> Sequent -> Sequent
(<|) a = ((0, a) :)
infixr 5 <|

-- | Check if there are zero F signed formulae
nullFs :: Sequent -> Bool
nullFs x = null [a | (_, F a) <- x]

-- | Replace all F signed formulae with the given formula
replaceFs :: Formula -> Sequent -> Sequent
replaceFs a x = (0, F a) : [b | b@(_, T _) <- x]

-- | Enqueue a signed formula into the sequent
insert :: SequentItem -> Sequent -> Sequent
insert = insertBy (\a b -> compare (fst a) (fst b))

-- | Substitute a signed formulae, reset order when updated
substi :: String -> Formula -> SequentItem -> SequentItem
substi s b (p, a) = case Just (s, b) `alter` unsign a of
  (True, c) -> (0, c <$ a)
  _         -> (p, a)
