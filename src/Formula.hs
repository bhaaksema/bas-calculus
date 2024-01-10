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
isV :: Formula -> Bool
isV (Var _) = True
isV _       = False

-- | Check if a formula is a conjunction
isC :: Formula -> Bool
isC (_ :& _) = True
isC _        = False

-- | Check if a formula is a disjunction
isD :: Formula -> Bool
isD (_ :| _) = True
isD _        = False

-- | Check if a formula is an implication
isI :: Formula -> Bool
isI (_ :> _) = True
isI _        = False

-- | Check if a formula is an implication with a variable on the left
isVI :: Formula -> Bool
isVI (a :> _) = isV a
isVI _        = False

-- | Check if a formula is an implication withithout a variable on the left
isXI :: Formula -> Bool
isXI a = isI a && not (isVI a)

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
