module Prover (prove) where

import           Bounding      (Axiom, for)
import           Data.List     (intersect, union)
import qualified Data.MultiSet as M
import           Formula       (Formula (..))

-- Type aliases
type Extension = (Formula -> [Formula], [Formula])
type Sequent = (M.MultiSet Formula, Formula)

-- | Inserts an element into a multiset
(+=) :: Ord a => M.MultiSet a -> a -> M.MultiSet a
(+=) = flip M.insert

-- | Deletes an element from a multiset
(-=) :: Ord a => M.MultiSet a -> a -> M.MultiSet a
(-=) = flip M.delete

-- | Checks if a formula is provable
prove :: [Axiom] -> Formula -> Bool
prove as f = unary (for as, for as f) (M.empty, f)

-- | First, check initial sequents and invertible rules with one premise
unary :: Extension -> Sequent -> Bool
unary _ (_, T) = True
unary as (facts, a :> b) = unary as (facts += a, b)
unary as@(bf, old) (facts, e) = initial e || leftUnary (M.toList facts) where
  initial (V _) = e `M.member` facts
  initial _     = False
  leftUnary (F : _ ) = True
  leftUnary (a :& b : _) = unary as (facts -= (a :& b) += a += b, e)
  leftUnary (a :> b : next) = let fs = facts -= (a :> b) in case a of
    V _    -> if a `M.member` facts then unary as (fs += b, e) else leftUnary next
    F      -> unary as (fs, e)
    T      -> unary as (fs += b, e)
    c :& d -> unary as (fs += (c :> (d :> b)), e)
    c :| d -> unary as (fs += (c :> b) += (d :> b), e)
    _      -> leftUnary next
  leftUnary (_ : next) = leftUnary next
  leftUnary [] = case M.toList facts of
    [f] -> cut (old `intersect` bf (f :> e))
    fs  -> cut (old `intersect` foldr (union . bf) (bf e) fs)
  cut (a : new) = unary (bf, new) (facts += a, e)
  cut []        = binary as (facts, e)

-- | Then, check invertible rules with two premises
binary :: Extension -> Sequent -> Bool
binary as (facts, a :& b) = unary as (facts, a) && ((a /= b) && unary as (facts, b))
binary as (facts, e) = leftOr (M.toList facts) where
  leftOr (a :| b : _) = let fs = facts -= (a :| b) in
    unary as (fs += a, e) && unary as (fs += b, e)
  leftOr (_ : fs)     = leftOr fs
  leftOr []           = noninv as (facts, e)

-- | Finally, check non-invertible rules
noninv :: Extension -> Sequent -> Bool
noninv as (facts, e) = rightOr e || any leftImp facts where
  rightOr (a :| b) = unary as (facts, a) || ((a /= b) && unary as (facts, b))
  rightOr _        = False
  leftImp ((c :> d) :> b) = let fs = facts -= ((c :> d) :> b) in
    unary as (fs += (d :> b), c :> d) && unary as (fs += b, e)
  leftImp _ = False
