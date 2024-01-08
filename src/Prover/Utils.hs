module Prover.Utils where

import Data.Bifunctor    (first, second)
import Data.List.Ordered (insertBag, member)
import Formula           (Formula)

type Sequent a = ([Formula], a)
type Stash a = Sequent a -> Sequent a
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
