{-# LANGUAGE FlexibleInstances #-}
module Prover.Classic (prove) where

import Data.Collection (Category (..))
import Data.Formula
import Data.Sequent

class Provable a where
  prove :: a -> Bool

-- | Check formula provability
instance Provable Formula where
  prove = prove . singletonR schL schR . simply where

    schL :: Formula -> Category
    schL Bot      = C0
    schL Top      = C0
    schL (Var _)  = C1
    schL (Neg _)  = C1
    schL (_ :& _) = C1
    schL (_ :| _) = C2
    schL (_ :> _) = C2

    schR :: Formula -> Category
    schR Top      = C0
    schR Bot      = C0
    schR (Var _)  = C1
    schR (Neg _)  = C1
    schR (_ :& _) = C2
    schR (_ :| _) = C1
    schR (_ :> _) = C1

-- | Check sequent provability
instance Provable Sequent where
  prove s1 = case view s1 of
    Just (sgn, f, s) -> case sgn of
      L -> case f of
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
      R -> case f of
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
