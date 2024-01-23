module Bounding (Axiom, embed) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Formula
import qualified Prover   as P

-- | Set of variables of a formula
vars :: Formula -> S.Set Formula
vars (a :& b) = vars a `S.union` vars b
vars (a :| b) = vars a `S.union` vars b
vars (a :> b) = vars a `S.union` vars b
vars (V str)  = S.singleton (V str)
vars _        = S.empty

-- | Set of subformulas of a formula
fors :: Formula -> S.Set Formula
fors (a :& b) = S.insert (a :& b) $ fors a `S.union` fors b
fors (a :| b) = S.insert (a :| b) $ fors a `S.union` fors b
fors (a :> b) = S.insert (a :> b) $ fors a `S.union` fors b
fors a        = S.singleton a

-- | Set of conjunctions of subformulas of a formula
cons :: Formula -> S.Set Formula
cons = S.map (foldl1 (:&)) . S.delete S.empty . S.powerSet . fors

-- | Axiom is kind of a formula
type Axiom = Formula

-- | Substitute all variables in a formula
vmap :: (String -> Formula) -> Formula -> Formula
vmap f (a :& b) = vmap f a :& vmap f b
vmap f (a :| b) = vmap f a :| vmap f b
vmap f (a :> b) = vmap f a :> vmap f b
vmap f (V str)  = f str
vmap _ a        = a

-- | Generalized bounding function
bfunc :: (Formula -> S.Set Formula) -> [Axiom] -> Formula -> S.Set Formula
bfunc f axi a = S.fromList [vmap (M.fromDistinctAscList m M.!) ax | ax <- axi, m <- maps ax]
 where maps ax = sequence [[(str, b) | b <- S.toList (f a)] | V str <- S.toList (vars ax)]

-- | Embed an intermediate logic into intuitionistic logic
embed :: [Axiom] -> Formula -> Formula
embed axi a = foldl1 (:&) (bfunc cons axi a) :> a

-- | Prove a superintuitionistic theorem
sprove :: [Axiom] -> Formula -> Bool
sprove ax = P.iprove . embed ax
