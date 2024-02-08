module Utils.Formula where

import qualified Data.Map as M

import Formula (Formula (..))

-- | \(O(n)\). Simplify formula without substitution
simply :: Formula -> Formula
simply = fullSubsti M.empty

-- | \(O(n)\). Simplify and fully apply substitution map
fullSubsti :: M.Map String Formula -> Formula -> Formula
fullSubsti = substi True

-- | \(O(n)\). Simplify and (partially) apply singlton substitution map
unitSubsti :: Bool -> (String, Formula) -> Formula -> Formula
unitSubsti t (p, a) = substi t (M.singleton p a)

-- | \(O(n)\). Simplify and (partially) apply a substitution map
substi :: Bool -> M.Map String Formula -> Formula -> Formula
substi t m (a1 :& b1)
  | a == b || b == Top = a
  | a == Top = b
  | a == Bot || b == Bot = Bot
  | otherwise = a :& b
  where a = substi t m a1; b = substi t m b1
substi t m (a1 :| b1)
  | a == b || b == Bot = a
  | a == Bot = b
  | a == Top || b == Top = Top
  | otherwise = a :| b
  where a = substi t m a1; b = substi t m b1
substi True m (a1 :> b1)
  | a == b || b == Top || a == Bot = Top
  | a == Top = b
  | otherwise = a :> b
  where a = substi True m a1; b = substi True m b1
substi _ m (Var p) | Just a <- m M.!? p = a
substi _ _ a = a
