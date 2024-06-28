{-# LANGUAGE FlexibleInstances, LambdaCase #-}
module Prover.Intuition (prove) where

import           Data.Formula
import           Data.Sequent
import qualified Prover.Classic as Cl

class Provable a where
  prove :: a -> Bool

-- | Check formula provability
instance Provable Formula where
  prove = prove . fromFormula (\case
    L -> \case
      Bot           -> C0; Top             -> C0
      (Var _)       -> C1; (Neg (Var _))   -> C1; (Neg (_ :| _))  -> C1
      (_ :& _)      -> C1; ((_ :& _) :> _) -> C1; ((_ :| _) :> _) -> C1
      (_ :| _)      -> C2; (Neg _ :> _)    -> C4; ((_ :> _) :> _) -> C4
      (Neg (Neg _)) -> C5; (Neg (_ :> _))  -> C5; (Neg (_ :& _))  -> C6
      _ -> CX
    R -> \case
      Top      -> C0; Bot      -> C0
      (_ :| _) -> C1; (_ :& _) -> C2
      (Neg _)  -> C3; (_ :> _) -> C3
      _ -> CX;
    )

-- | Check sequent provability
instance Provable Sequent where
  prove r | nullR r = Cl.prove (toFormula r)
  prove r = case view r of
    Just (L, f, s) -> case f of
      -- Category 0
      Bot -> True
      Top -> prove s
      a | member R a s -> True
      -- Category 1
      Var p -> prove (subst p Top s)
      Neg (Var p) -> prove (subst p Bot s)
      a :& b -> prove (add L a $ add L b s)
      Neg (a :| b) -> prove (add L (Neg a) $ add L (Neg b) s)
      (a :& b) :> c -> prove (add L (a :> b :> c) s)
      (a :| b) :> c
        | (p, t) <- fresh s ->
        prove (add L (a :> p) $ add L (b :> p) $ add L (p :> c) t)
      -- Category 2
      a :| b -> all prove [add L a s, add L b s]
      a :> b
        | member L a s -> prove (add L b s)
        | not (Cl.prove (toFormula r)) -> False
      -- Category 4
      Neg a :> b
        | not (prove (add L b s)) -> False
        | prove (add L a $ delR s) -> True
      (a :> b) :> c
        | not (prove (add L c s)) -> False
        | (p, t) <- fresh s
        , prove (add L a $ add L (b :> p) $ add L (p :> c) $ setR p t)
        -> True
      -- Category 5
      Neg (Neg a) -> prove (add L a $ delR s)
      Neg (a :> b) -> prove (add L a $ add L (Neg b) $ delR s)
      -- Category 6
      Neg (a :& b) -> all (\c -> prove (add L (Neg c) $ delR s)) [a, b]
      -- Backtrack
      _ -> prove (lock L f s)
    Just (R, f, s) -> case f of
      -- Category 0
      Top -> True
      Bot -> prove s
      a | member L a s -> True
      -- Category 1
      a :| b -> prove (add R a $ add R b s)
      -- Category 2
      a :& b -> all prove [add R a s, add R b s]
      -- Category 3
      Neg a | prove (add L a $ delR s) -> True
      a :> b | prove (add L a $ setR b s) -> True
      -- Backtrack
      _ -> not (nullR s) && prove (lock R f s)
    Nothing -> False
