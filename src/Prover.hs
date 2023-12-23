module Prover (prove) where

import Control.Monad.Extra (anyM, (&&^), (||^))
import Control.Monad.State (State, evalState, get, put)

import Bounding  (set)
import Data.List (intersect)
import Formula   (Formula (..))
import Utils     (holes)

type ProofState = State ([Formula], [Formula])

-- | Checks if a formula is provable
prove :: [Formula] -> Formula -> Bool
prove as f = evalState (initial [] f) (as, set as f)

-- | First, check initial sequents
initial :: [Formula] -> Formula -> ProofState Bool
initial facts e = let fs = filter (/= T) facts in
  pure (e == T || any (\f -> f == e || f == F) fs) ||^ unary fs e

-- | Then, check invertible rules with one premise
unary :: [Formula] -> Formula -> ProofState Bool
unary facts (a :> b) = initial (a : facts) b
unary facts (a :| b)
  | a == b    = initial facts a
  | otherwise = pure (a == T || b == T) ||^ binary facts (a :| b)
unary facts e = leftUnary (holes facts) where
  leftUnary ((a :& b, fs) : _) = initial (a : b : fs) e
  leftUnary ((a :> b, fs) : next) = case a of
    Var _  -> if a `elem` fs then initial (b : fs) e else leftUnary next
    F      -> initial fs e
    T      -> initial (b : fs) e
    c :& d -> initial ((c :> (d :> b)) : fs) e
    c :| d -> initial ((c :> b) : (d :> b) : fs) e
    _      -> leftUnary next
  leftUnary (_ : next) = leftUnary next
  leftUnary [] = binary facts e

-- | Next, check invertible rules with two premises
binary :: [Formula] -> Formula -> ProofState Bool
binary facts (a :& b)
  | a == b    = initial facts a
  | otherwise = initial facts a &&^ initial facts b
binary facts e = leftOr (holes facts) where
  leftOr ((a :| b, fs) : _) = initial (a : fs) e &&^ initial (b : fs) e
  leftOr (_ : next)         = leftOr next
  leftOr []                 = noninv facts e

-- | Finally, check non-invertible rules
noninv :: [Formula] -> Formula -> ProofState Bool
noninv facts (a :| b) = initial facts a ||^ initial facts b
noninv facts e = anyM leftImp (holes facts) ||^ do
  (as, inst) <- get
  cut as (inst `intersect` set as (foldr (:>) e facts))
  where
  leftImp ((c :> d) :> b, fs) = initial (d :> b : fs) (c :> d) &&^ initial (b : fs) e
  leftImp _ = pure False
  cut as (a : inst) = put (as, inst) >> initial (a : facts) e
  cut _ []          = pure False
