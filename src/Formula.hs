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
  | c :& d <- a = c :> d :> b
  | c :| d <- a = (c :> b) :& (d :> b)
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
data SignedFormula = T Formula | F Formula
  deriving (Eq, Show)

-- | Ord instance for SignedFormula
instance Ord SignedFormula where
  compare a b | a == b = EQ

  compare (T Bot) _ = GT
  compare (F Top) _ = GT
  compare _ (T Bot) = LT
  compare _ (F Top) = LT

  compare (T (Var _)) _ = GT
  compare (T (Var _ :> _)) _ = GT
  compare (T (_ :& _)) _ = GT
  compare (F (_ :| _)) _ = GT
  compare _ (T (Var _)) = LT
  compare _ (T (Var _ :> _)) = LT
  compare _ (T (_ :& _)) = LT
  compare _ (F (_ :| _)) = LT

  compare (F (_ :> _)) _ = GT
  compare _ (F (_ :> _)) = LT

  compare (F (_ :& _)) _ = GT
  compare (T (_ :| _)) _ = GT
  compare _ (F (_ :& _)) = LT
  compare _ (T (_ :| _)) = LT

  compare (T (_ :> _)) _ = GT
  compare _ (T (_ :> _)) = LT

  compare _ _ = LT
