module Formula where

-- | Propositional formula
data Formula = Var String | F | T
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

-- | Check if a formula is a variable
isVar :: Formula -> Bool
isVar (Var _) = True
isVar _       = False

-- | Show instance for Formula
instance Show Formula where
  show = showBi where
    showUn (Var v) = v
    showUn F       = "⊥"
    showUn T       = "⊤"
    showUn f       = "(" ++ show f ++ ")"
    showBi (a :& b) = showUn a ++ " ∧ " ++ showUn b
    showBi (a :| b) = showUn a ++ " ∨ " ++ showUn b
    showBi (a :> b) = showUn a ++ " → " ++ showUn b
    showBi f        = showUn f
