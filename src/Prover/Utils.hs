module Prover.Utils where

import Data.List.Ordered (insertBag, member)
import Formula           (Formula)

type Sequent a = ([Formula], a)
type Stash a = Sequent a -> Sequent a

(!:) :: Formula -> [Formula] -> [Formula]
(!:) = insertBag

infixr 8 !:

(!?) :: Formula -> [Formula] -> Bool
(!?) = member
