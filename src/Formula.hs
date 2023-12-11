module Formula (Formula (..), neg, iff, vars, formulas, fusions) where
import Data.List (subsequences, union)

data Formula = T
             | F
             | Var String
             | And Formula Formula
             | Or Formula Formula
             | Imp Formula Formula
             deriving (Eq, Ord)

neg :: Formula -> Formula
neg f = Imp f F

iff :: Formula -> Formula -> Formula
iff f g = And (Imp f g) (Imp g f)

-- | Get all unique variables in a formula
vars :: Formula -> [Formula]
vars (Var s)   = [Var s]
vars (And f g) = vars f `union` vars g
vars (Or f g)  = vars f `union` vars g
vars (Imp f g) = vars f `union` vars g
vars _         = []

formulas :: Formula -> [Formula]
formulas (Var s)   = [Var s]
formulas (And f g) = formulas f `union` formulas g `union` [And f g]
formulas (Or f g)  = formulas f `union` formulas g `union` [Or f g]
formulas (Imp f g) = formulas f `union` formulas g `union` [Imp f g]
formulas f         = [f]

fusions :: Formula -> [Formula]
fusions = map fuse . tail . subsequences . formulas
  where fuse = foldr1 And

-- | Show instance for Formula
instance Show Formula where
  show (And f g) = sf f ++ " ∧ " ++  sf g
    where
      sf x@(And _ _) = show x
      sf x           = showSF x
  show (Or f g)  = sf f ++ " ∨ " ++ sf g
    where
      sf x@(Or _ _) = show x
      sf x           = showSF x
  show (Imp f g) = showSF f ++ " → " ++ showSF g
  show f         = showSF f

showSF :: Formula -> String
showSF T         = "⊤"
showSF F         = "⊥"
showSF (Var s)   = s
showSF f         = "(" ++ show f ++ ")"
