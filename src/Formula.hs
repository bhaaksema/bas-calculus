module Formula where

import qualified Data.Map as M

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
simplify = subst M.empty

-- | Substitute and reduce a formula
subst :: M.Map String Formula -> Formula -> Formula
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
subst f (Var s) | Just a <- f M.!? s = simplify a
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
