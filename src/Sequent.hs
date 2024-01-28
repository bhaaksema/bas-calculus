module Sequent where

import           Data.Bifunctor (bimap)
import qualified Data.Either    as E
import qualified Data.List      as L
import           Formula

type PFormula = (Int, SFormula)
type Sequent = [PFormula]

singleton :: SFormula -> Sequent
singleton a = [(0, a)]

rights :: Sequent -> [Formula]
rights = E.rights . map snd

left :: Formula -> Sequent -> Sequent
left a s = (0, Left a) : s
infixr 5 `left`

right :: Formula -> Sequent -> Sequent
right a s = (0, Right a) : s
infixr 5 `right`

setRight :: Formula -> [PFormula] -> [PFormula]
setRight a = ((0, Right a) :) . filter (E.isLeft . snd)

insert :: PFormula -> Sequent -> Sequent
insert = L.insert

sub :: (String, Formula) -> [PFormula] -> [PFormula]
sub f = map (bimap (const 0) (bimap (subst f) (subst f)))
