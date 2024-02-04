module Sequent where

import Data.Sequence as S

import Formula

-- | Sign for propositional formula
data SignedFormula = T Formula | F Formula
  deriving (Eq, Show)

-- | Sequent is a ordered set of signed formulae
type Sequent = Seq (Word, SignedFormula)

-- | Sequent with one signed formula
singleton :: SignedFormula -> Sequent
singleton a = a <|^ empty

-- | Inspect the sequence
view :: Sequent -> Maybe ((Word, SignedFormula), Sequent)
view ((a :<| x) :|> b) | fst b < fst a = Just (b, a :<| x)
view (a :<| x) = Just (a, x)
view Empty = Nothing

-- | Insert a signed formula with initial priority
(<|^) :: SignedFormula -> Sequent -> Sequent
a <|^ x = (0, a) :<| x
infixr <|^

-- | Insert a signed formula with provided priority
insert :: (Word, SignedFormula) -> Sequent -> Sequent
insert a ((b :<| x) :|> c) | fst c > fst b = (b :<| x) :|> c :|> a
insert a (b :<| x) = a :<| b :<| x
insert a Empty = a :<| Empty

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
