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
  showsPrec _ (Var s) = showString s
  showsPrec _ Bot = showChar '⊥'
  showsPrec _ Top = showChar '⊤'
  showsPrec p (a :& b) = showParen (p > 8) $
    showsPrec 8 a . showString " ∧ " . showsPrec 8 b
  showsPrec p (a :| b) = showParen (p > 7) $
    showsPrec 7 a . showString " ∨ " . showsPrec 7 b
  showsPrec _ (a :> Bot) = showChar '¬' . showsPrec 9 a
  showsPrec p (a :> b) = showParen (p > 6) $
    showsPrec 7 a . showString " → " . showsPrec 6 b

-- | Sign for a propositional formula
data Sign a = T {unsign :: a} | F {unsign :: a}
  deriving (Eq, Show)

-- | Functor instance for Sign
instance Functor Sign where
  fmap f (T a) = T $ f a
  fmap f (F a) = F $ f a
