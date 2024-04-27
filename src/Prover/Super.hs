module Prover.Super (Axiomatization (..), prove, proveWith) where

import qualified Data.IntMap as M
import qualified Data.Set    as S

import           Data.Formula
import qualified Prover.Intuition as Int

-- | Axiomatization types
data Axiomatization = VAR | FOR | SET deriving (Eq, Ord, Show)

-- | Set of variables of a formula
vars :: Formula -> S.Set Formula
vars p@(Var _) = S.singleton p
vars (a :& b)  = vars a `S.union` vars b
vars (a :| b)  = vars a `S.union` vars b
vars (a :> b)  = vars a `S.union` vars b
vars _         = S.empty

-- | Set of subformulas of a formula
fors :: Formula -> S.Set Formula
fors (a :& b) = S.insert (a :& b) $ fors a `S.union` fors b
fors (a :| b) = S.insert (a :| b) $ fors a `S.union` fors b
fors (a :> b) = S.insert (a :> b) $ fors a `S.union` fors b
fors a        = S.singleton a

-- | Set of conjunctions of subformulas of a formula
cons :: Formula -> S.Set Formula
cons = S.map (foldr1 (:&)) . S.deleteMin . S.powerSet . fors

-- | Convert Axiomatization to corresponding formula
toFunction :: Axiomatization -> (Formula -> S.Set Formula)
toFunction VAR = vars
toFunction FOR = fors
toFunction SET = cons

-- Upwards propagation
upwards :: (Formula -> Formula -> Formula) -> Formula -> Formula -> Bool
upwards op a p = let
  q = succ p; r = succ q
  subap b = unitSubsti True (fromEnum p, b) a
  in Int.prove (subap q :& subap r :> subap (op q r))

-- Check the kind of axiomatization
axiomatization :: Formula -> Axiomatization
axiomatization a | allUp (:&) = if allUp (:|) && allUp (:>) then VAR else FOR
  where allUp op = all (upwards op a) (vars a)
axiomatization _ = SET

-- | Generalized bounding function
bfunc :: S.Set Formula -> [Formula] -> [Formula]
bfunc set as = let
  in [fullSubsti m a | a <- as,
  m <- foldr (\p -> (M.insert (fromEnum p) <$> S.toList set <*>)) [M.empty] (vars a)]

-- | Prove with specified axiomatization type
proveWith :: Axiomatization -> [Formula] -> Formula -> Bool
proveWith axiom as f = let
  set = toFunction axiom f
  in Int.prove (foldr (:&) Top (bfunc set as) :> f)

-- | Prove a superintuitionistic theorem via intuitionistic logic embedding
prove :: [Formula] -> Formula -> Bool
prove axioms = proveWith (maximum $ map axiomatization axioms) axioms
