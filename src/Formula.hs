{-# LANGUAGE DeriveFunctor #-}
module Formula where

-- | Propositional formula
data Formula = Bot | Top
  | Var String
  | Formula :& Formula
  | Formula :| Formula
  | Formula :> Formula
  deriving (Eq, Ord)
infix 8 :&
infix 7 :|
infixr 6 :>

-- | Negation of formula
neg :: Formula -> Formula
neg a = a :> Bot

-- | Bi-implication of two formulae
(<:>) :: Formula -> Formula -> Formula
(<:>) a b = (a :> b) :& (b :> a)
infix 5 <:>

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

-- | Sign for propositional formula
data Sign a = T a | F a
  deriving (Eq, Ord, Show, Functor)

-- | \(O(1)\). Check if the sign is T
isT :: Sign a -> Bool
isT (T _) = True; isT _ = False

-- | \(O(1)\). Check if the sign is F
isF :: Sign a -> Bool
isF (F _) = True; isF _ = False
