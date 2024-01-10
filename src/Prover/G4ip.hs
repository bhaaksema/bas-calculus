module Prover.G4ip where

import           Formula
import           MultiSet    ((<.), (>.))
import qualified MultiSet    as M
import qualified Prover.G3cp as C

-- | Single succedent sequent
type Sequent = (M.MultiSet, Formula)

leftInvImp :: Formula -> Bool
leftInvImp (V _ :> _) = False
leftInvImp (a :> _)   = not (C.isI a)
leftInvImp _          = False

leftVarImp :: M.MultiSet -> Formula -> Bool
leftVarImp x (V s :> _) = s `M.vmember` x
leftVarImp _ _          = False

leftImpImp :: Sequent -> Formula -> Bool
leftImpImp (x, y) a@((c :> d) :> b) = prove1 (d :> b >. x1, c :> d) && prove1 (b >. x1, y) where x1 = M.delete a x
leftImpImp _ _ = False

-- | Prove a intuitionistic theorem
prove :: Formula -> Bool
prove a = prove1 (M.empty, a)

-- | Check axioms and rules
prove1 :: Sequent -> Bool
prove1 (x, y)
  -- Initial sequent
  | V s <- y, s `M.vmember` x = True
  | M.unF x || y == T = True
  -- Glivenko's optimisation
  | F <- y = C.prove1 (x, M.singleton F)
  -- Right implication
  | a :> b <- y = prove1 (a >. x, b)
  -- Left conjunction
  | Just (a :& b, x1) <- C.isC <. x = prove1 (a >. b >. x1, y)
  -- Left implication (invertible)
  | Just (F :> _, x1)        <- leftInvImp <. x = prove1 (x1, y)
  | Just (T :> b, x1)        <- leftInvImp <. x = prove1 (b >. x1, y)
  | Just ((c :& d) :> b, x1) <- leftInvImp <. x = prove1 (c :> (d :> b) >. x1, y)
  | Just ((c :| d) :> b, x1) <- leftInvImp <. x = prove1 (c :> b >. d :> b >. x1, y)
  -- Right conjunction
  | a :& b <- y = prove1 (x, a) && prove1 (x, b)
  -- Left disjunction
  | Just (a :| b, x1) <- C.isD <. x = prove1 (a >. x1, y) && prove1 (b >. x1, y)
  -- Left implication (invertible cont.)
  | Just (_ :> b, x1) <- leftVarImp x <. x = prove1 (b >. x1, y)
  -- Right disjunction (non-invertible)
  | a :| b <- y, prove1 (x, a) || prove1 (x, b) = True
  -- Left implication (non-invertible)
  | Just _ <- leftImpImp (x, y) <. x = True
  | otherwise = False
