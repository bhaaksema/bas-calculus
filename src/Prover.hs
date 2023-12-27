module Prover (prove) where

import Bounding
import Data.List (delete, intersect)
import Formula   (Formula (..))

type CutFormulas = (Formula -> [Formula], [Formula])

update :: CutFormulas -> [Formula] -> Formula -> CutFormulas
update (bf, inst) facts e = (bf, inst `intersect` bf (foldr (:>) e facts) `intersect` bf (foldl (:&) T facts :> e))

-- | Checks if a formula is provable
prove :: [Formula] -> Formula -> Bool
prove as f = unary (for as, for as f) [] f

-- | First, check initial sequents and invertible rules with one premise
unary :: CutFormulas -> [Formula] -> Formula -> Bool
unary _ _ T = True
unary as facts (a :> b) = unary as (a : facts) b
unary as facts e = initial e || leftUnary facts where
  initial (Var _) = e `elem` facts
  initial _       = False
  leftUnary (F : _ ) = True
  leftUnary (a :& b : _) = unary as (a : b : delete (a :& b) facts) e
  leftUnary (a :> b : fs) = case a of
    Var _  -> if a `elem` facts then unary as (b : delete (a :> b) facts) e else leftUnary fs
    F      -> unary as (delete (a :> b) facts) e
    T      -> unary as (b : delete (a :> b) facts) e
    c :& d -> unary as ((c :> (d :> b)) : delete (a :> b) facts) e
    c :| d -> unary as ((c :> b) : (d :> b) : delete (a :> b) facts) e
    _      -> leftUnary fs
  leftUnary (_ : next) = leftUnary next
  leftUnary [] = cut (update as facts e)
  cut (axi, a : inst) = unary (axi, inst) (a : facts) e
  cut (_, [])         = binary as facts e

-- | Then, check invertible rules with two premises
binary :: CutFormulas -> [Formula] -> Formula -> Bool
binary as facts (a :& b)
  | a == b    = unary as facts a
  | otherwise = unary as facts a && unary as facts b
binary as facts e = leftOr facts where
  leftOr (a :| b : _) = unary as (a : fs) e && unary as (b : fs) e
    where fs = delete (a :| b) facts
  leftOr (_ : fs)     = leftOr fs
  leftOr []           = noninv as facts e

-- | Finally, check non-invertible rules
noninv :: CutFormulas -> [Formula] -> Formula -> Bool
noninv as facts e = rightOr e || any leftImp facts where
  rightOr (a :| b)
    | a == b    = unary as facts a
    | otherwise = unary as facts a || unary as facts b
  rightOr _ = False
  leftImp ((c :> d) :> b) = unary as (d :> b : fs) (c :> d) && unary as (b : fs) e
    where fs = delete ((c :> d) :> b) facts
  leftImp _ = False
