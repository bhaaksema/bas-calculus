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
    Just (Left (f, s)) -> case f of
        -- Category 0
        Bot    -> True
        Top    -> prove s
        -- Category 1
        Var p  -> prove (subst True p Top s)
        Neg a  -> prove (addR a s)
        a :& b -> prove (addL a $ addL b s)
        -- Category 2
        a :| b -> all prove [addL a s, addL b s]
        a :> b -> all prove [addR a s, addL b s]
    Just (Right (f, s)) -> case f of
        -- Category 0
        Top    -> True
        Bot    -> prove s
        -- Category 1
        Var p  -> prove (subst True p Bot s)
        Neg a  -> prove (addL a s)
        a :| b -> prove (addR a $ addR b s)
        a :> b -> prove (addL a $ addR b s)
        -- Category 2
        a :& b -> all prove [addR a s, addR b s]
    Nothing -> False
