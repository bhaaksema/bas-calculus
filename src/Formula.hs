module Formula where

import qualified Data.Map as M

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

-- | Simplify formula
simply :: Formula -> Formula
simply = alter M.empty

-- | Simplify and apply one substitution
alter1 :: String -> Formula -> Formula -> Formula
alter1 s a = alter (M.singleton s a)

-- | Simplify and apply a substitution map
alter :: M.Map String Formula -> Formula -> Formula
alter m (a1 :& b1)
  | a == b || b == Top = a
  | a == Top = b
  | a == Bot || b == Bot = Bot
  | otherwise = a :& b
  where a = alter m a1; b = alter m b1
alter m (a1 :| b1)
  | a == b || b == Bot = a
  | a == Bot = b
  | a == Top || b == Top = Top
  | otherwise = a :| b
  where a = alter m a1; b = alter m b1
alter m (a1 :> b1)
  | a == b || b == Top || a == Bot = Top
  | a == Top = b
  | otherwise = a :> b
  where a = alter m a1; b = alter m b1
alter m (Var s) | Just a <- m M.!? s = a
alter _ a = a

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
  deriving (Eq, Show)

-- | Functor instance for Sign
instance Functor Sign where
  fmap f (T a) = T $ f a
  fmap f (F a) = F $ f a
