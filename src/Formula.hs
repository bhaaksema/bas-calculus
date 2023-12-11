module Formula (Formula (..), neg, iff, vars, formulas, fusions) where
import Data.List (subsequences, union)

-- | Propositional formula
data Formula = T
             | F
             | Var String
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

-- | Get all unique variables of a formula
vars :: Formula -> [Formula]
vars (Var s)   = [Var s]
vars (And f g) = vars f `union` vars g
vars (Or f g)  = vars f `union` vars g
vars (Imp f g) = vars f `union` vars g
vars _         = []

-- | Get all unique subformulas of a formula
formulas :: Formula -> [Formula]
formulas (Var s)   = [Var s]
formulas (And f g) = formulas f `union` formulas g `union` [And f g]
formulas (Or f g)  = formulas f `union` formulas g `union` [Or f g]
formulas (Imp f g) = formulas f `union` formulas g `union` [Imp f g]
formulas f         = [f]

-- | Get all unique fusions of subformulas of a formula
fusions :: Formula -> [Formula]
fusions = map fuse . tail . subsequences . formulas
  where fuse = foldr1 And

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
showSF (Var s) = s
showSF f       = "(" ++ show f ++ ")"
