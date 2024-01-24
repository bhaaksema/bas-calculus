module Formula where

-- | Propositional formula
data Formula
  = Var String | Bot | Top
  | Formula :& Formula
  | Formula :| Formula
  | Formula :> Formula
  deriving (Eq, Ord)
infix 8 :&
infix 7 :|
infixr 6 :>

-- | Negation of a formula
neg :: Formula -> Formula
neg a = a :> Bot

-- | Bi-implication of two formulas
iff :: Formula -> Formula -> Formula
iff a b = (a :> b) :& (b :> a)
infix 5 `iff`

-- | Simplify a formula
simplify :: Formula -> Formula
simplify (a :& b)
  | p == q || q == Top = p
  | p == Top = q
  | p == Bot || q == Bot = Bot
  | otherwise = p :& q
  where p = simplify a; q = simplify b
simplify (a :| b)
  | p == q || q == Bot = p
  | p == Bot = q
  | p == Top || q == Top = Top
  | otherwise = p :| q
  where p = simplify a; q = simplify b
simplify (a :> b)
  | p == q || q == Top || p == Bot = Top
  | p == Top = q
  | otherwise = p :> q
  where p = simplify a; q = simplify b
simplify p = p

-- | Show instance for Formula
instance Show Formula where
  show x
    | a :& b <- x = showUn a ++ " ∧ " ++ showUn b
    | a :| b <- x = showUn a ++ " ∨ " ++ showUn b
    | a :> b <- x, b /= Bot = showUn a ++ " → " ++ showUn b
    | otherwise = showUn x where
    showUn (Var s)    = s
    showUn Bot        = "⊥"
    showUn Top        = "⊤"
    showUn (a :> Bot) = '¬' : showUn a
    showUn y          = '(' : show y ++ ")"
