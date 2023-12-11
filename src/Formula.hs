module Formula (Formula(..), neg, iff) where
import           Data.List (intercalate)

-- | Propositional formulae
data Formula = Var String
             | And [Formula]
             | Or [Formula]
             | Imp Formula Formula
             | F
             | T
             deriving (Eq, Ord)

neg :: Formula -> Formula
neg f = Imp f F

iff :: Formula -> Formula -> Formula
iff f g = And [Imp f g, Imp g f]

-- | Show instance for Formula
instance Show Formula where
  show (And fs)  = intercalate " ∧ " $ map showSF fs
  show (Or fs)   = intercalate " ∨ " $ map showSF fs
  show (Imp f F) = "¬" ++ showSF f
  show (Imp f g) = showSF f ++ " → " ++ showSF g
  show f         = showSF f

showSF :: Formula -> String
showSF (Var s)   = s
showSF T         = "⊤"
showSF F         = "⊥"
showSF (Imp f F) = "¬" ++ showSF f
showSF f         = "(" ++ show f ++ ")"
