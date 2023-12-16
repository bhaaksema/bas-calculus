module Formula (Formula (..), neg, iff) where

-- | Propositional formula
data Formula = Var String
             | F
             | T
             | And Formula Formula
             | Or Formula Formula
             | Imp Formula Formula
             deriving (Eq, Ord)

-- | Negation of a formula
neg :: Formula -> Formula
neg f = f `Imp` F

-- | Bi-implication of two formulas
iff :: Formula -> Formula -> Formula
iff f g = (f `Imp` g) `And` (g `Imp` f)

-- | Show instance for Formula
instance Show Formula where
  show (And f g) = sf f ++ " ∧ " ++  sf g
    where
      sf x@(And _ _) = show x
      sf x           = showSF x
  show (Or f g) = sf f ++ " ∨ " ++ sf g
    where
      sf x@(Or _ _) = show x
      sf x          = showSF x
  show (Imp f g) = showSF f ++ " → " ++ showSF g
  show f         = showSF f

showSF :: Formula -> String
showSF T       = "⊤"
showSF F       = "⊥"
showSF (Var v) = v
showSF f       = "(" ++ show f ++ ")"
