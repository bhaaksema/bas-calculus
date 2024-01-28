module Formula where

-- | Propositional formula
data Formula = Bot | Top
  | Var String
  | Formula :> Formula
  | Formula :| Formula
  | Formula :& Formula
  deriving (Eq, Ord)
infixr 6 :>
infix 7 :|
infix 8 :&

-- | Signed propositional formula
type SFormula = Either Formula Formula

-- | Negation of a formula
neg :: Formula -> Formula
neg a = a :> Bot

-- | Bi-implication of two formulas
(<:>) :: Formula -> Formula -> Formula
(<:>) a b = (a :> b) :& (b :> a)
infix 5 <:>

-- | Simplify a formula
simplify :: Formula -> Formula
simplify = subst ("", Var "")

-- | Substitute and reduce a formula
subst :: (String, Formula) -> Formula -> Formula
subst f (a1 :& b1)
  | a == b || b == Top = a
  | a == Top = b
  | a == Bot || b == Bot = Bot
  | otherwise = a :& b
  where a = subst f a1; b = subst f b1
subst f (a1 :| b1)
  | a == b || b == Bot = a
  | a == Bot = b
  | a == Top || b == Top = Top
  | otherwise = a :| b
  where a = subst f a1; b = subst f b1
subst f (a1 :> b1)
  | a == b || b == Top || a == Bot = Top
  | a == Top = b
  | c :& d <- a = c :> (d :> b)
  | c :| d <- a = (c :> b) :& (d :> b)
  | otherwise = a :> b
  where a = subst f a1; b = subst f b1
subst (s1, a) (Var s2) | s1 == s2 = a
subst _ a = a

-- | Show instance for Formula
instance Show Formula where
  show c
    | a :& b <- c = showUn a ++ " ∧ " ++ showUn b
    | a :| b <- c = showUn a ++ " ∨ " ++ showUn b
    | a :> b <- c, b /= Bot = showUn a ++ " → " ++ showUn b
    | otherwise = showUn c where
    showUn (Var s)    = s
    showUn Bot        = "⊥"
    showUn Top        = "⊤"
    showUn (a :> Bot) = '¬' : showUn a
    showUn y          = '(' : show y ++ ")"
