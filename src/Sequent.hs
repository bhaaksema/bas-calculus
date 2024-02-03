module Sequent where

import qualified Data.Set as S

import Formula

-- | Sign for propositional formula
data SignedFormula = T Formula | F Formula
  deriving (Eq, Show)

-- | Ord instance for SignedFormula
instance Ord SignedFormula where
  compare a b
    | a == b = EQ
    -- Linear conclusion
    | T Bot <- a = GT
    | F Top <- a = GT
    | T Bot <- b = LT
    | F Top <- b = LT
    | T (Var _) <- a = GT
    | T (Var _ :> _) <- a = GT
    | T (_ :& _) <- a = GT
    | F (_ :| _) <- a = GT
    | T (Var _) <- b = LT
    | T (Var _ :> _) <- b = LT
    | T (_ :& _) <- b = LT
    | F (_ :| _) <- b = LT
    | F (_ :> _) <- a = GT
    | F (_ :> _) <- b = LT
    -- Branching conclusion
    | F (_ :& _) <- a = GT
    | T (_ :| _) <- a = GT
    | F (_ :& _) <- b = LT
    | T (_ :| _) <- b = LT
    | T (_ :> _) <- a = GT
    | T (_ :> _) <- b = LT
    | otherwise = LT

-- | Sequent is a ordered set of signed formulae
type Sequent = S.Set (Bool, SignedFormula)

-- | Sequent with one signed formula
single :: SignedFormula -> Sequent
single a = a <| S.empty

-- | Insert a signed formula with initial priority
(<|) :: SignedFormula -> Sequent -> Sequent
a <| x = S.insert (True, a) x
infixr <|

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
substi p c = substi' S.empty where
  substi' x se = case S.minView se of
    Just (a, z) -> let b = smap (alter1 p c) (snd a) in
      substi' (if b == snd a then S.insert a x else b <| x) z
      where smap f (T d) = T $ f d; smap f (F d) = F $ f d
    Nothing -> x
