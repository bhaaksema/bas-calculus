module Sequent where

import qualified Data.Set as S

import Formula

-- | Sequent is a ordered set of signed formulae
type Sequent = S.Set (Word, SignedFormula)

-- | Sequent with one signed formula
single :: SignedFormula -> Sequent
single a = a <| S.empty

-- | Cleverly insert a signed formula with priority
(<|) :: SignedFormula -> Sequent -> Sequent
a <| _ | a `elem` [T Bot, F Top] = S.singleton (0, a)
a <| x | a `elem` [T Top, F Bot] = x
F (a :| b) <| x = F a <| F b <| x
T (a :& b) <| x = T a <| T b <| x
a <| x = S.insert (0, a) x
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
    Nothing -> x
