module Prover.G4ip where

import           Formula
import           MultiSet    ((>.))
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
  | Var v <- y, v `M.vmember` x = True
  | M.unF x || y == T = True
  | F <- y = C.prove1 (x, M.singleton F)
  | a :> b <- y = prove1 (a >. x, b)
  | Just (a :& b, x1) <- M.pop isAnd x = prove1 (a >. b >. x1, y)
  | Just (Var v :> b, x1) <- M.pop isVImp x, v `M.vmember` x = prove1 (b >. x1, y)
  | Just (F :> _, x1) <- M.pop isOImp x = prove1 (x1, y)
  | Just (T :> b, x1) <- M.pop isOImp x = prove1 (b >. x1, y)
  | Just (c :& d :> b, x1) <- M.pop isOImp x = prove1 (c :> (d :> b) >. x1, y)
  | Just (c :| d :> b, x1) <- M.pop isOImp x = prove1 (c :> b >. d :> b >. x1, y)
  | a :& b <- y = prove1 (x, a) && ((a /= b) && prove1 (x, b))
  | Just (a :| b, x1) <- M.pop isOr x = prove1 (a >. x1, y) && ((a /= b) && prove1 (b >. x1, y))
  | a :| b <- y, prove1 (x, a) || ((a /= b) && prove1 (x, b)) = True
  | Just ((c :> d) :> b, x1) <- M.pop isOImp x = prove1 (d :> b >. x1, c :> d) && prove1 (b >. x1, y)
  | otherwise = False
