module Formula where

import qualified Data.IntMap as M

-- | Propositional formula
data Formula = Bot | Top
  | Var Int
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

-- | Gets a fresh variable for a formula
fresh :: Formula -> Formula
fresh Bot      = Var 0
fresh Top      = Var 0
fresh (Var p)  = Var (p + 1)
fresh (Neg a)  = fresh a
fresh (a :& b) = max (fresh a) (fresh b)
fresh (a :| b) = max (fresh a) (fresh b)
fresh (a :> b) = max (fresh a) (fresh b)

-- | Simplify formula without substitution
simply :: Formula -> Formula
simply = fullSubsti M.empty

-- | Simplify and fully apply substitution map
fullSubsti :: M.IntMap Formula -> Formula -> Formula
fullSubsti = substi True

-- | Simplify and (partially) apply singlton substitution map
unitSubsti :: Bool -> (Int, Formula) -> Formula -> Formula
unitSubsti t (p, f) = substi t (M.singleton p f)

-- | Apply boolean simplification rules
-- and (partially) apply a substitution map
substi :: Bool -> M.IntMap Formula -> Formula -> Formula
substi _ m (Var p) | Just f <- m M.!? p = f
substi True m (Neg a1) = case
  fullSubsti m a1 of
    Bot -> Top
    Top -> Bot
    a   -> Neg a
substi t m (a1 :& b1) = case
  (substi t m a1, substi t m b1) of
    (Bot, _) -> Bot
    (_, Bot) -> Bot
    (Top, b) -> b
    (a, Top) -> a
    (a, b)   -> a :& b
substi t m (a1 :| b1) = case
  (substi t m a1, substi t m b1) of
    (Bot, b) -> b
    (a, Bot) -> a
    (Top, _) -> Top
    (_, Top) -> Top
    (a, b)   -> a :| b
substi True m (a1 :> b1) = case
  (fullSubsti m a1, fullSubsti m b1) of
    (Bot, _) -> Top
    (a, Bot) -> Neg a
    (Top, b) -> b
    (_, Top) -> Top
    (a, b)   -> a :> b
substi _ _ a = a

-- | Show instance for Formula
instance Show Formula where
  showsPrec _ Bot = showChar '⊥'
  showsPrec _ Top = showChar '⊤'
  showsPrec _ (Var p) = showString (show p)
  showsPrec _ (Neg a) = showChar '¬' . showsPrec 9 a
  showsPrec p (a :& b) = showParen (p > 8) $
    showsPrec 8 a . showString " ∧ " . showsPrec 8 b
  showsPrec p (a :| b) = showParen (p > 7) $
    showsPrec 7 a . showString " ∨ " . showsPrec 7 b
  showsPrec p (a :> b) = showParen (p > 6) $
    showsPrec 7 a . showString " → " . showsPrec 6 b
