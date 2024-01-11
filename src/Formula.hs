module Formula where

import qualified Data.Set as S

-- | Propositional formula
data Formula = V String | F | T
  | Formula :& Formula
  | Formula :| Formula
  | Formula :> Formula
  deriving (Eq, Ord)

-- | Negation of a formula
neg :: Formula -> Formula
neg a = a :> F

-- | Bi-implication of two formulas
iff :: Formula -> Formula -> Formula
iff a b = (a :> b) :& (b :> a)

-- | Get all unique variables of a formula
vars :: Formula -> S.Set String
vars (a :& b) = vars a `S.union` vars b
vars (a :| b) = vars a `S.union` vars b
vars (a :> b) = vars a `S.union` vars b
vars (V str)  = S.singleton str
vars _        = S.empty

-- | Get all unique subformulas of a formula
fors :: Formula -> S.Set Formula
fors (a :& b) = S.insert (a :& b) $ fors a `S.union` fors b
fors (a :| b) = S.insert (a :| b) $ fors a `S.union` fors b
fors (a :> b) = S.insert (a :> b) $ fors a `S.union` fors b
fors a        = S.singleton a

-- | Show instance for Formula
instance Show Formula where
  show = showBi where
    showUn (V v) = v
    showUn F     = "⊥"
    showUn T     = "⊤"
    showUn f     = "(" ++ show f ++ ")"
    showBi (a :& b) = showUn a ++ " ∧ " ++ showUn b
    showBi (a :| b) = showUn a ++ " ∨ " ++ showUn b
    showBi (a :> b) = showUn a ++ " → " ++ showUn b
    showBi f        = showUn f
