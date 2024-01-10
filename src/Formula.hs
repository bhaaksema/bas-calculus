module Formula where

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

-- | Check if variable
isV :: Formula -> Bool
isV (V _) = True
isV _     = False

-- | Check if conjunction
isC :: Formula -> Bool
isC (_ :& _) = True
isC _        = False

-- | Check if disjunction
isD :: Formula -> Bool
isD (_ :| _) = True
isD _        = False

-- | Check if implication
isI :: Formula -> Bool
isI (_ :> _) = True
isI _        = False

-- | Check if (V s :> b)
isVI :: Formula -> Bool
isVI (a :> _) = isV a
isVI _        = False

-- | Check if (a :> b), not (isVI a), not (isI a)
isXI :: Formula -> Bool
isXI (a :> _) = not (isI a || isVI a)
isXI _        = False

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
