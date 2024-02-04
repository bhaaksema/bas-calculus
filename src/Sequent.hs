module Sequent where

import Data.Sequence as S hiding ((<|))

import Formula

-- | Sign for propositional formula
data SignedFormula = T Formula | F Formula
  deriving (Eq, Show)

-- | Sequent is a ordered set of signed formulae
type Sequent = Seq (Word, SignedFormula)

-- | Sequent with one signed formula
single :: SignedFormula -> Sequent
single a = a <|^ empty

-- | Insert a signed formula with initial priority
(<|^) :: SignedFormula -> Sequent -> Sequent
a <|^ x = (0, a) :<| x
infixr <|^

-- | Check if the sequent contains no F-signed formulae
nullFs :: Sequent -> Bool
nullFs x = S.null $ S.filter isF x
  where isF (_, F _) = True; isF _ = False

-- | Replace all F-signed formulae with one given formula
replaceFs :: Formula -> Sequent -> Sequent
replaceFs a x = F a <|^ S.filter isT x
  where isT (_, T _) = True; isT _ = False

-- | Substitute Sequent, reset formula priority when changed
substi :: String -> Formula -> Sequent -> Sequent
substi p c = substi' S.empty where
  substi' x Empty = x
  substi' x (a :<| y) | b <- smap (alter1 p c) (snd a) =
    substi' (if b == snd a then x :|> a else b <|^ x) y
    where smap f (T d) = T $ f d; smap f (F d) = F $ f d
