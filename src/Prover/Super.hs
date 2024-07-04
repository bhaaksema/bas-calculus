module Prover.Super where

import qualified Data.IntMap as M
import qualified Data.Set    as S

import           Data.Formula
import qualified Prover.Intuition as Int
import Debug.Trace

-- | Set of variables of a formula
vars :: Formula -> S.Set Formula
vars p@(Var _) = S.singleton p
vars (Neg a)   = vars a
vars (a :& b)  = vars a `S.union` vars b
vars (a :| b)  = vars a `S.union` vars b
vars (a :> b)  = vars a `S.union` vars b
vars _         = S.empty

-- | Set of subformulas of a formula
fors :: Formula -> S.Set Formula
fors (Neg a)  = S.insert (Neg a) $ fors a
fors (a :& b) = S.insert (a :& b) $ fors a `S.union` fors b
fors (a :| b) = S.insert (a :| b) $ fors a `S.union` fors b
fors (a :> b) = S.insert (a :> b) $ fors a `S.union` fors b
fors a        = S.singleton a

-- | Set of conjunctions of subformulas of a formula
cons :: Formula -> S.Set Formula
cons = S.map (foldr1 (:&)) . S.deleteMin . S.powerSet . fors

-- | Axiomatisation types
data Axiomatisation = VAR | FOR | SET deriving (Eq, Ord, Show)

-- | Convert Axiomatisation to corresponding function
toFunction :: Axiomatisation -> (Formula -> S.Set Formula)
toFunction VAR = vars
toFunction FOR = fors
toFunction SET = cons

-- Upwards propagation
upwards :: (Formula -> Formula -> Formula) -> Formula -> Formula -> Bool
upwards op a p = let
  q = succ a; r = succ q
  substitution b = substitute1 (fromEnum p) b a
  in Int.prove (substitution q :& substitution r :> substitution (op q r))

-- Check the kind of axiomatisation
axiomatisation :: Formula -> Axiomatisation
axiomatisation a | allUp (:&) = if allUp (:|) && allUp (:>) then VAR else FOR
  where allUp op = all (upwards op a) (vars a)
axiomatisation _ = SET

-- | Generalized bounding function
bounding :: S.Set Formula -> [Formula] -> [Formula]
bounding set as =
  [ substitute m a | a <- as
  , m <- foldr
    (\p -> (M.insert (fromEnum p) <$> S.toList set <*>))
    [M.empty] (vars a)
  ]

-- | Prove with specified axiomatisation type
proveWith :: Axiomatisation -> [Formula] -> Formula -> Bool
proveWith axiom as goal = let
  set = toFunction axiom goal
  in Int.prove (foldr (:&) Top (bounding set as) :> goal)

-- | Prove a superintuitionistic theorem via intuitionistic logic embedding
prove :: [Formula] -> Formula -> Bool
prove axioms =
  trace (show $ map axiomatisation axioms)
  proveWith (maximum $ map axiomatisation axioms) axioms
