module Prover (prove) where

import Bounding  (for, set)
import Data.List (delete, intersect)
import Formula   (Formula (..))

type CutFormulas = ([Formula], [Formula])

update :: CutFormulas -> [Formula] -> Formula -> CutFormulas
update (axi, inst) facts e = (axi, inst `intersect` for axi (foldr (:>) e facts) `intersect` for axi (foldl (:&) T facts :> e))

-- | Checks if a formula is provable
prove :: [Formula] -> Formula -> Bool
prove as f = initial (as, for as f) [] f

-- | First, check initial sequents
initial :: CutFormulas -> [Formula] -> Formula -> Bool
initial as facts e = let fs = filter (/= T) facts in
  e == T || any (\f -> f == e || f == F) fs || unary as fs e

-- | Then, check invertible rules with one premise
unary :: CutFormulas -> [Formula] -> Formula -> Bool
unary as facts (a :> b) = initial as (a : facts) b
unary as facts e = leftUnary facts where
  leftUnary (a :& b : _) = initial as (a : b : delete (a :& b) facts) e
  leftUnary (a :> b : fs) = case a of
    Var _  -> if a `elem` facts then initial as (b : delete (a :> b) facts) e else leftUnary fs
    F      -> initial as (delete (a :> b) facts) e
    T      -> initial as (b : delete (a :> b) facts) e
    c :& d -> initial as ((c :> (d :> b)) : delete (a :> b) facts) e
    c :| d -> initial as ((c :> b) : (d :> b) : delete (a :> b) facts) e
    _      -> leftUnary fs
  leftUnary (_ : next) = leftUnary next
  leftUnary [] = binary as facts e

-- | Next, check invertible rules with two premises
binary :: CutFormulas -> [Formula] -> Formula -> Bool
binary as facts (a :& b)
  | a == b    = initial as facts a
  | otherwise = initial as facts a && initial as facts b
binary as facts e = leftOr facts where
  leftOr (a :| b : _) = initial as (a : fs) e && initial as (b : fs) e
    where fs = delete (a :| b) facts
  leftOr (_ : fs)     = leftOr fs
  leftOr []           = noninv as facts e

-- | Finally, check non-invertible rules
noninv :: CutFormulas -> [Formula] -> Formula -> Bool
noninv as facts e = rightOr e || any leftImp facts || cut as where
  rightOr (a :| b)
    | a == b    = initial as facts a
    | otherwise = initial as facts a || initial as facts b
  rightOr _ = False
  leftImp ((c :> d) :> b) = initial as (d :> b : fs) (c :> d) && initial as (b : fs) e
    where fs = delete ((c :> d) :> b) facts
  leftImp _ = False
  cut (axi, a : inst) = initial (axi, inst) (a : facts) e
  cut (_, [])         = False
