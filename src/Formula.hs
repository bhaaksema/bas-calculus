module Formula where

-- | Propositional formula
data Formula = Var String | Bot | Top
  | Formula :& Formula
  | Formula :| Formula
  | Formula :> Formula
  deriving (Eq, Ord)
infix 8 :&
infix 7 :|
infixr 6 :>

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
simply :: Formula -> Formula
simply = snd . alter Nothing

-- | Reduce a formula and possibly apply a substitution
alter :: Maybe (String, Formula) -> Formula -> (Bool, Formula)
alter f (a1 :& b1)
  | a == b || b == Top = (True, a)
  | a == Top = (True, b)
  | a == Bot || b == Bot = (True, Bot)
  | otherwise = (u1 || u2, a :& b)
  where (u1, a) = alter f a1; (u2, b) = alter f b1
alter f (a1 :| b1)
  | a == b || b == Bot = (True, a)
  | a == Bot = (True, b)
  | a == Top || b == Top = (True, Top)
  | otherwise = (u1 || u2, a :| b)
  where (u1, a) = alter f a1; (u2, b) = alter f b1
alter f (a1 :> b1)
  | a == b || b == Top || a == Bot = (True, Top)
  | a == Top = (True, b)
  | otherwise = (u1 || u2, a :> b)
  where (u1, a) = alter f a1; (u2, b) = alter f b1
alter (Just (s1, a)) (Var s2) | s1 == s2 = (True, a)
alter _ a = (False, a)

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
