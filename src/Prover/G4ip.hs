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
prove1 (x, y)
  -- Initial sequent
  | Var v <- y, v `M.vmember` x = True
  | M.unF x || y == T = True
  -- Glivenko's theorem
  | F <- y = C.prove1 (x, M.singleton F)
  -- Right implication
  | a :> b <- y = prove1 (a >. x, b)
  -- Left conjunction
  | Just (a :& b, x1) <- isA <. x = prove1 (a >. b >. x1, y)
  -- Left implication (invertible)
  | Just (Var v :> b, x1)  <- isVI <. x, v `M.vmember` x = prove1 (b >. x1, y)
  | Just (F :> _, x1)      <- isXI <. x = prove1 (x1, y)
  | Just (T :> b, x1)      <- isXI <. x = prove1 (b >. x1, y)
  | Just (c :& d :> b, x1) <- isXI <. x = prove1 (c :> (d :> b) >. x1, y)
  | Just (c :| d :> b, x1) <- isXI <. x = prove1 (c :> b >. d :> b >. x1, y)
  -- Right conjunction
  | a :& b <- y = prove1 (x, a) && prove1 (x, b)
  -- Left disjunction
  | Just (a :| b, x1) <- isO <. x = prove1 (a >. x1, y) && prove1 (b >. x1, y)
  -- Right disjunction
  | a :| b <- y, prove1 (x, a) || prove1 (x, b) = True
  -- Left implication (non-invertible)
  | Just ((c :> d) :> b, x1) <- isXI <. x = prove1 (d :> b >. x1, c :> d) && prove1 (b >. x1, y)
  | otherwise = False
