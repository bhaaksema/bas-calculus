{-# LANGUAGE FlexibleInstances, LambdaCase #-}
module Prover.Classic (prove) where

import Data.Collection (Category (..))
import Data.Formula
import Data.Sequent

class Provable a where
  prove :: a -> Bool

-- | Check formula provability
instance Provable Formula where
  prove = prove . fromFormula (\case
    L -> \case
      Bot      -> C0; Top      -> C0
      (Var _)  -> C1; (Neg _)  -> C1; (_ :& _) -> C1
      (_ :| _) -> C2; (_ :> _) -> C2
    R -> \case
      Top      -> C0; Bot     -> C0;
      (Var _)  -> C1; (Neg _) -> C1; (_ :| _)  -> C1; (_ :> _) -> C1
      (_ :& _) -> C2
    )

-- | Check sequent provability
instance Provable Sequent where
  prove r = case view r of
    Just (L, f, s) -> case f of
      -- Category 0
      Bot    -> True
      Top    -> prove s
      -- Category 1
      Var p  -> prove (subst True p Top s)
      Neg a  -> prove (add R a s)
      a :& b -> prove (add L a $ add L b s)
      -- Category 2
      a :| b -> all prove [add L a s, add L b s]
      a :> b -> all prove [add R a s, add L b s]
    Just (R, f, s) -> case f of
      -- Category 0
      Top    -> True
      Bot    -> prove s
      -- Category 1
      Var p  -> prove (subst True p Bot s)
      Neg a  -> prove (add L a s)
      a :| b -> prove (add R a $ add R b s)
      a :> b -> prove (add L a $ add R b s)
      -- Category 2
      a :& b -> all prove [add R a s, add R b s]
    Nothing -> False
