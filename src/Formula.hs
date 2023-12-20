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
neg a = a :> F

-- | Bi-implication of two formulas
iff :: Formula -> Formula -> Formula
iff a b = (a :> b) :& (a :> b)

-- | Show instance for Formula
instance Show Formula where
  show (a :& b) = showSF a ++ " ∧ " ++ showSF b
  show (a :| b) = showSF a ++ " ∨ " ++ showSF b
  show (a :> b) = showSF a ++ " → " ++ showSF b
  show a        = showSF a

showSF :: Formula -> String
showSF (Var v) = v
showSF F       = "⊥"
showSF T       = "⊤"
showSF a       = "(" ++ show a ++ ")"
