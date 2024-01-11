{-# LANGUAGE LambdaCase #-}
module Prover.G4ip where

import           Formula
import           MultiSet    ((+>), (<+))
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
  | V s <- y, s `M.vmember` x = True
  | M.unF x || y == T = True
  -- Glivenko's optimisation
  | F <- y = C.prove1 (x, M.singleton F)
  -- Right implication
  | a :> b <- y = prove1 (a +> x, b)
  -- Left conjunction
  | Just (a :& b, x1) <- C.isC <+ x = prove1 (a +> b +> x1, y)
  -- Left implication
  | Just (V _ :> b, x1) <- isI <+ x = prove1 (b +> x1, y)
  | Just (F :> _, x1) <- isI <+ x = prove1 (x1, y)
  | Just (T :> b, x1) <- isI <+ x = prove1 (b +> x1, y)
  | Just ((c :& d) :> b, x1) <- isI <+ x = prove1 (c :> (d :> b) +> x1, y)
  | Just ((c :| d) :> b, x1) <- isI <+ x = prove1 (c :> b +> d :> b +> x1, y)
  -- Right conjunction
  | a :& b <- y = prove1 (x, a) && prove1 (x, b)
  -- Left disjunction
  | Just (a :| b, x1) <- C.isD <+ x = prove1 (a +> x1, y) && prove1 (b +> x1, y)
  -- Left implication (cont.)
  | Just _ <- (\case e@((_ :> _) :> b) -> not (prove1 (b +> M.delete e x, y)); _ -> False) <+ x = False
  | Just _ <- (\case e@(a@(_ :> d) :> b) -> prove1 (d :> b +> M.delete e x, a); _ -> False) <+ x = True
  -- Right disjunction
  | a :| b <- y, prove1 (x, a) || prove1 (x, b) = True
  -- Left implication (alt.)
  -- | Just (_ :> b, x1) <- (\case e@(a@(_ :> d) :> b) -> prove1 (d :> b +> M.delete e x, a); _ -> False) <+ x = prove1 (b +> x1, y)
  | otherwise = False
  where isI = \case (V s :> _) -> s `M.vmember` x; (a :> _) -> not (C.isI a); _ -> False
