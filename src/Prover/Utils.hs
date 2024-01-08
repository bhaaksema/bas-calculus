module Prover.Utils where

import Data.Bifunctor    (first, second)
import Data.List.Ordered (insertBag, member)
import Formula           (Formula)

type Sequent a = ([Formula], a)
type Stash a = ([Formula], a)
type M = [Formula]
type S = Formula

(<:) :: Formula -> Sequent a -> Sequent a
(<:) = first . insertBag
infixr 8 <:

(>:) :: Formula -> Sequent M -> Sequent M
(>:) = second . insertBag
infixr 8 >:

(!?) :: Formula -> [Formula] -> Bool
(!?) = member

pop :: Stash M -> Sequent M -> Sequent M
pop (f1, f2) (x, y) = (reverse f1 ++ x, reverse f2 ++ y)
