module Formula (Formula (..), neg, iff) where

-- | Propositional formula
data Formula = Var String
             | F
             | T
             | Formula :& Formula
             | Formula :| Formula
             | Formula :> Formula
             deriving (Eq, Ord)

-- | Negation of a formula
neg :: Formula -> Formula
neg f = f :> F

-- | Bi-implication of two formulas
iff :: Formula -> Formula -> Formula
iff f g = (f :> g) :& (g :> f)

-- | Show instance for Formula
instance Show Formula where
  show (f :& g) = sf f ++ " ∧ " ++  sf g
    where
      sf x@(_ :& _) = show x
      sf x          = showSF x
  show (f :| g) = sf f ++ " ∨ " ++ sf g
    where
      sf x@(_ :| _) = show x
      sf x          = showSF x
  show (f :> g) = showSF f ++ " → " ++ showSF g
  show f = showSF f

showSF :: Formula -> String
showSF (Var v) = v
showSF F       = "⊥"
showSF T       = "⊤"
showSF f       = "(" ++ show f ++ ")"
