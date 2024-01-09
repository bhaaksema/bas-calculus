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

-- | Check if a formula is a conjunction
isAnd :: Formula -> Bool
isAnd (_ :& _) = True
isAnd _        = False

-- | Check if a formula is a disjunction
isOr :: Formula -> Bool
isOr (_ :| _) = True
isOr _        = False

-- | Check if a formula is an implication
isImp :: Formula -> Bool
isImp (_ :> _) = True
isImp _        = False

-- | Check if a formula is an implication with a variable on the left
isVImp :: Formula -> Bool
isVImp (a :> _) = isVar a
isVImp _        = False

-- | Check if a formula is an implication withithout a variable on the left
isOImp :: Formula -> Bool
isOImp a = isImp a && not (isVImp a)

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
