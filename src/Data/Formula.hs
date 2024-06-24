module Data.Formula where

import qualified Data.IntMap as M

-- | Propositional formula
data Formula
  = Bot | Top | Var Int
  | Neg Formula
  | Formula :& Formula
  | Formula :| Formula
  | Formula :> Formula
  deriving (Eq, Ord)
infixl 8 :&
infixl 7 :|
infixr 6 :>

-- | Bi-implication of two formulae
(<:>) :: Formula -> Formula -> Formula
(<:>) a b = (a :> b) :& (b :> a)
infix 5 <:>

-- | Substitution of a single variable
substitute1 :: Int -> Formula -> Formula -> Formula
substitute1 variable formula = substitute (M.singleton variable formula)

-- | Variable substitution function,
-- also applies boolean simplification rules
substitute :: M.IntMap Formula -> Formula -> Formula
substitute m (Var p) | Just formula <- m M.!? p = formula
substitute m (Neg a1) = case
  substitute m a1 of
    Bot -> Top
    Top -> Bot
    a   -> Neg a
substitute m (a1 :& b1) = case
  (substitute m a1, substitute m b1) of
    (Bot, _) -> Bot
    (_, Bot) -> Bot
    (Top, b) -> b
    (a, Top) -> a
    (a, b)   -> a :& b
substitute m (a1 :| b1) = case
  (substitute m a1, substitute m b1) of
    (Bot, b) -> b
    (a, Bot) -> a
    (Top, _) -> Top
    (_, Top) -> Top
    (a, b)   -> a :| b
substitute m (a1 :> b1) = case
  (substitute m a1, substitute m b1) of
    (Bot, _) -> Top
    (a, Bot) -> Neg a
    (Top, b) -> b
    (_, Top) -> Top
    (a, b)   -> a :> b
substitute _ a = a

-- | Enum instance for Formula
instance Enum Formula where
  toEnum = Var
  fromEnum Bot      = 0
  fromEnum Top      = 0
  fromEnum (Var n)  = n
  fromEnum (Neg a)  = fromEnum a
  fromEnum (a :& b) = fromEnum a `max` fromEnum b
  fromEnum (a :| b) = fromEnum a `max` fromEnum b
  fromEnum (a :> b) = fromEnum a `max` fromEnum b

-- | Show instance for Formula
instance Show Formula where
  showsPrec _ Bot = showString "$false"
  showsPrec _ Top = showString "$true"
  showsPrec _ (Var p) = showChar 'p' . shows p
  showsPrec _ (Neg a) = showChar '~' . showsPrec 9 a
  showsPrec p (a :& b) = showParen (p > 8) $
    showsPrec 8 a . showString " & " . showsPrec 8 b
  showsPrec p (a :| b) = showParen (p > 7) $
    showsPrec 7 a . showString " | " . showsPrec 7 b
  showsPrec p (a :> b) = showParen (p > 6) $
    showsPrec 7 a . showString " => " . showsPrec 6 b
