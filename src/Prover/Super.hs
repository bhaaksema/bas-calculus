module Prover.Super (prove) where

import qualified Data.IntMap as M
import qualified Data.Set    as S

import           Data.Formula
import qualified Prover.Intuition as Int

-- | Axiomatization kinds
data Axiomatization = VAR | FOR | SET
  deriving (Eq, Ord, Show)

-- | Set of variables of a formula
vars :: Formula -> S.Set Int
vars (Var p)  = S.singleton p
vars (a :& b) = vars a `S.union` vars b
vars (a :| b) = vars a `S.union` vars b
vars (a :> b) = vars a `S.union` vars b
vars _        = S.empty

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
toFunction VAR = S.map Var . vars
toFunction FOR = fors
toFunction SET = cons

-- Upwards propagation
upwards :: (Formula -> Formula -> Formula) -> Formula -> Formula -> Int -> Bool
upwards op ax q p | r <- succ q = Int.prove (subap q :& subap r :> subap (op q r))
  where subap b = unitSubsti True (p, b) ax

-- Check the kind of axiomatization
axiomatization :: Formula -> Axiomatization
axiomatization ax | allUp (:&)
  = if allUp (:|) && allUp (:>) then VAR else FOR
  where allUp op = all (upwards op ax (succ ax)) (vars ax)
axiomatization _ = SET

-- | Generalized bounding function
bfunc :: [Formula] -> Formula -> [Formula]
bfunc axioms f = let
  fs = (toFunction $ maximum $ map axiomatization axioms) f
  in [fullSubsti m ax | ax <- axioms,
  m <- foldr (\p -> (M.insert p <$> S.toList fs <*>)) [M.empty] (vars ax)]

-- | Prove a superintuitionistic theorem via intuitionistic logic embedding
prove :: [Formula] -> Formula -> Bool
prove axioms f = Int.prove (foldr (:&) Top (bfunc axioms f) :> f)
