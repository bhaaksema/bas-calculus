module Prover.G4ip where

import           Formula
import           MultiSet    ((<.), (>.))
import qualified MultiSet    as M
import qualified Prover.G3cp as C

-- | Single succedent sequent
type Sequent = (M.MultiSet, Formula)

-- | Prove a intuitionistic theorem
prove :: Formula -> Bool
prove a = prove1 (M.empty, a)

-- | Check axioms and rules
prove1 :: Sequent -> Bool
prove1 (z, y)
  -- Initial sequent
  | V s <- y, s `M.vmember` x = True
  | M.unF x || y == T = True
  -- Glivenko's optimisation
  | F <- y = C.prove1 (x, M.singleton F)
  -- Right implication
  | a :> b <- y = prove1 (a >. x, b)
  -- Left conjunction
  | Just (a :& b, x1) <- isC <. x = prove1 (a >. b >. x1, y)
  -- Left implication (invertible)
  | Just (F :> _, x1)        <- isXI <. x = prove1 (x1, y)
  | Just (T :> b, x1)        <- isXI <. x = prove1 (b >. x1, y)
  | Just ((c :& d) :> b, x1) <- isXI <. x = prove1 (c :> (d :> b) >. x1, y)
  | Just ((c :| d) :> b, x1) <- isXI <. x = prove1 (c :> b >. d :> b >. x1, y)
  -- Right conjunction
  | a :& b <- y = prove1 (x, a) && prove1 (x, b)
  -- Left disjunction
  | Just (a :| b, x1) <- isD <. x = prove1 (a >. x1, y) && prove1 (b >. x1, y)
  | otherwise = prove2 (x, y)
  where x = M.unstash z

-- | Check special rules
prove2 :: Sequent -> Bool
prove2 (x, y)
  -- Left implication (invertible cont.)
  | Just (a@(V s :> b), x1) <- isVI <. x =
    if s `M.vmember` x then prove1 (b >. x1, y) else prove2 (M.stash a x1, y)
  -- Right disjunction (non-invertible)
  | a :| b <- y, prove1 (x, a) || prove1 (x, b) = True
  -- Left implication (non-invertible)
  | Just (a@((c :> d) :> b), x1) <- const True <. x =
    (prove1 (d :> b >. x1, c :> d) && prove1 (b >. x1, y)) || prove2 (M.stash a x1, y)
  | otherwise = False
