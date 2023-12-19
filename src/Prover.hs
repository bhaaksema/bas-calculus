module Prover (prove) where

import Formula

right :: [Formula] -> [Formula] -> [Formula] -> Formula -> Bool
right vars facts ys goal = goal `elem` vars || case goal of  -- Initial sequent
  T      -> True                                             -- Right top
  a :& b -> right vars facts ys a && right vars facts ys b   -- Right conjunction
  a :> b -> right vars (a : facts) ys b                      -- Right implication
  _      -> left vars facts ys goal

left :: [Formula] -> [Formula] -> [Formula] -> Formula -> Bool
left vars facts ys goal = case facts of
  (v@(Var _) : fs) -> right (v : vars) fs ys goal
  (F : _) -> True                                            -- Left bottom
  (T : fs) -> right vars fs ys goal                          -- Left top
  (a :& b : fs) -> right vars (a : b : fs) ys goal           -- Left conjunction
  (a :| b : fs) -> right vars (a : fs) ys goal && right vars (b : fs) ys goal -- Left disjunction
  (a :> b : fs) -> case a of
    F      -> right vars fs ys goal                          -- Left bottom implication
    T      -> right vars (b : fs) ys goal                    -- Left top implication
    c :& d -> right vars ((c :> (d :> b)) : fs) ys goal      -- Left conjunction implication
    c :| d -> right vars ((c :> b) : (d :> b) : fs) ys goal  -- Left disjunction implication
    _      -> right vars fs (a :> b : ys) goal               -- Store non-invertible
  [] -> nonInv vars ys goal                                  -- Non-invertible rules

nonInv :: [Formula] -> [Formula] -> Formula -> Bool
nonInv vars facts goal
  -- Right disjunction
  | case goal of
    (a :| b) -> right vars [] facts a || right vars [] facts b
    _        -> False = True
  | otherwise = any (\(f, fs) -> case f of
    -- Left variable implication (TODO: make this rule invertible)
    ((v@(Var _) :> b)) -> v `elem` vars && right vars [b] fs goal
    -- Left implication implication
    ((c :> d) :> b) -> right vars [d :> b] fs (c :> d) && right vars [b] fs goal
    _ -> False
  ) $ holes facts

holes :: [a] -> [(a, [a])]
holes []     = []
holes (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- holes xs]

prove :: Formula -> Bool
prove = right [] [] []
