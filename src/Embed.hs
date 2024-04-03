module Embed (Axiom, embed) where

import qualified Data.IntMap as M
import qualified Data.Set    as S

import Formula

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
cons = S.map (foldl1 (:&)) . S.deleteMin . S.powerSet . fors

-- | Axiom is kind of a formula
type Axiom = Formula

-- | Generalized bounding function
bfunc :: [Axiom] -> S.Set Formula -> [Formula]
bfunc axi as = [fullSubsti m ax | ax <- axi,
  m <- foldr (\p -> (M.insert p <$> S.toList as <*>)) [M.empty] (vars ax)]

-- | Embed an intermediate logic into intuitionistic logic
embed :: [Axiom] -> Formula -> Formula
embed axi f = foldl (:&) Top (bfunc axi (cons f)) :> f
