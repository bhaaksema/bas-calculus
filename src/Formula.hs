module Formula where

import qualified Data.Set as S

-- | Propositional formula
data Formula
  = V String -- ^ Propositional variable
  | F -- ^ Bottom constant
  | Formula :& Formula -- ^ Conjunction
  | Formula :| Formula -- ^ Disjunction
  | Formula :> Formula -- ^ Implication
  deriving (Eq, Ord)
infix 8 :&
infix 7 :|
infixr 6 :>

-- | Negation of a formula
neg :: Formula -> Formula
neg a = a :> F

-- | Bi-implication of two formulas
iff :: Formula -> Formula -> Formula
iff a b = (a :> b) :& (b :> a)
infix 5 `iff`

-- | Top constant
top :: Formula
top = neg F

-- | Set of variables of a formula
vars :: Formula -> S.Set String
vars (a :& b) = vars a `S.union` vars b
vars (a :| b) = vars a `S.union` vars b
vars (a :> b) = vars a `S.union` vars b
vars (V str)  = S.singleton str
vars _        = S.empty

-- | Set of subformulas of a formula
fors :: Formula -> S.Set Formula
fors (a :& b) = S.insert (a :& b) $ fors a `S.union` fors b
fors (a :| b) = S.insert (a :| b) $ fors a `S.union` fors b
fors (a :> b) = S.insert (a :> b) $ fors a `S.union` fors b
fors a        = S.singleton a

-- | Set of conjunctions of subformulas of a formula
cons :: Formula -> S.Set Formula
cons = S.map (foldl1 (:&)) . S.delete S.empty . S.powerSet . fors

-- | Show instance for Formula
instance Show Formula where
  show x
    | a :& b <- x = showUn a ++ " ∧ " ++ showUn b
    | a :| b <- x = showUn a ++ " ∨ " ++ showUn b
    | a :> b <- x, b /= F = showUn a ++ " → " ++ showUn b
    | otherwise = showUn x where
    showUn (V str)  = str
    showUn F        = "⊥"
    showUn (F :> F) = "⊤"
    showUn (a :> F) = '¬' : showUn a
    showUn y        = '(' : show y ++ ")"
