{-# LANGUAGE DeriveFunctor #-}
module Sequent where

import qualified Data.Set as S

import Formula (Formula, alter1)

-- | Sign for propositional formula
data Sign a = T a | F a
  deriving (Eq, Ord, Show, Functor)

-- | Sequent is a ordered set of signed formulae
type SequentItem = (Word, Sign Formula)
type Sequent = S.Set SequentItem

-- | Sequent with one signed formula
singleton :: Sign Formula -> Sequent
singleton a = a <| S.empty

-- | Inspect the sequence
view :: Sequent -> Maybe (SequentItem, Sequent)
view = S.minView

-- | Insert a signed formula with initial priority
(<|) :: Sign Formula -> Sequent -> Sequent
(<|) a = insert (0, a)
infixr <|

-- | Insert a signed formula with provided priority
insert :: SequentItem -> Sequent -> Sequent
insert = S.insert

-- | Check if the sequent contains no F-signed formulae
nullFs :: Sequent -> Bool
nullFs x = S.null $ S.filter isF x
  where isF (_, F _) = True; isF _ = False

-- | Replace all F-signed formulae with one given formula
replaceFs :: Formula -> Sequent -> Sequent
replaceFs a x = F a <| S.filter isT x
  where isT (_, T _) = True; isT _ = False

-- | Substitute Sequent, reset formula priority when changed
substi :: String -> Formula -> Sequent -> Sequent
substi p c = S.map (\(i, a) -> let b = alter1 p c <$> a
  in (if a == b then i else 0, b))
