module Prover (prove) where

import Data.List (delete)
import Formula

right :: [Formula] -> [Formula] -> [Formula] -> Formula -> Bool
right vars facts ys goal = goal `elem` vars || case goal of
  T -> True
  a :& b -> right vars facts ys a && right vars facts ys b
  a :> b -> right vars (a : facts) ys b
  _ -> case facts of
    (v@(Var _) : fs) -> right (v : vars) fs ys goal
    (F : _) -> True
    (T : fs) -> right vars fs ys goal
    (a :& b : fs) -> right vars (a : b : fs) ys goal
    (a :| b : fs) -> right vars (a : fs) ys goal && right vars (b : fs) ys goal
    (a :> b : fs) -> case a of
      F      -> right vars fs ys goal
      T      -> right vars (b : fs) ys goal
      c :& d -> right vars ((c :> (d :> b)) : fs) ys goal
      c :| d -> right vars ((c :> b) : (d :> b) : fs) ys goal
      _      -> right vars fs (a :> b : ys) goal
    [] -> any (\f -> left vars facts (delete f ys) f goal) ys || rightOr vars facts ys goal

rightOr :: [Formula] -> [Formula] -> [Formula] -> Formula -> Bool
rightOr vars facts ys (a :| b) = right vars facts ys a || right vars facts ys b
rightOr _ _ _ _                = False

left :: [Formula] -> [Formula] -> [Formula] -> Formula -> Formula -> Bool
left vars facts ys fact goal = case fact of
  ((v@(Var _) :> b)) -> v `elem` vars && right vars (b : facts) ys goal
  ((c :> d) :> b) -> right vars (d :> b : facts) ys (c :> d) && right vars (b : facts) ys goal
  _ -> False

prove :: Formula -> Bool
prove = right [] [] []
