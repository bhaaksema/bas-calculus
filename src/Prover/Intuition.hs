module Prover.Intuition (prove) where

import           Data.Collection (Category (..))
import           Data.Formula
import           Data.Sequent
import qualified Prover.Classic  as Cl

-- | Check formula provability
prove :: Formula -> Bool
prove = (\f -> iprove f (singletonR schL schR f)) . simply where

  schL :: Formula -> Category
  schL Bot             = C0
  schL Top             = C0
  schL (Var _)         = C1
  schL (Neg (Var _))   = C1
  schL (Neg (_ :| _))  = C1
  schL (_ :& _)        = C1
  schL ((_ :& _) :> _) = C1
  schL ((_ :| _) :> _) = C1
  schL (_ :| _)        = C2
  schL (Neg _ :> _)    = C4
  schL ((_ :> _) :> _) = C4
  schL (Neg (Neg _))   = C5
  schL (Neg (_ :> _))  = C5
  schL (Neg (_ :& _))  = C6
  schL _               = CX

  schR :: Formula -> Category
  schR Top      = C0
  schR Bot      = C0
  schR (Var _)  = C1
  schR (_ :| _) = C1
  schR (_ :& _) = C2
  schR (Neg _)  = C3
  schR (_ :> _) = C3

-- | Check sequent provability
iprove :: Formula -> Sequent -> Bool
iprove _ s | nullR s = Cl.prove (toFormula s)
iprove p s1 = case view s1 of
  Just (Left (f, s)) -> case f of
    -- Category 0
    Bot -> True
    Top -> iprove p s
    -- Category 1
    Var q -> iprove p (subst True q Top s)
    a :& b -> iprove p (addL a $ addL b s)
    Neg (a :| b) -> iprove p (addL (Neg a) $ addL (Neg b) s)
    (a :& b) :> c -> iprove p (addL (a :> b :> c) s)
    (a :| b) :> c -> let q = fresh p in
      iprove q (addL (a :> q) $ addL (b :> q) $ addL (q :> c) s)
    -- Category 2
    a :| b -> all (iprove p) [addL a s, addL b s]
    -- Category 4
    _ :> _ | not $ Cl.prove (toFormula s1) -> False
    Neg a :> b | nullR s ||
      iprove p (addL a $ unlock $ delR s) -> iprove p (addL b $ unlock s)
    (a :> b) :> c | q <- fresh p, nullR s ||
      iprove q (addL a $ addL (b :> q) $ addL (q :> c) $ setR q $ unlock s)
      -> iprove p (addL c $ unlock s)
    -- Category 5
    Neg (Neg a) -> iprove p (addL a $ unlock $ delR s)
    Neg (a :> b) -> iprove p (addL a $ addL (Neg b) $ unlock $ delR s)
    -- Category 6
    Neg (a :& b) -> all (\c -> iprove p (addL (Neg c) $ unlock $ delR s)) [a, b]
    -- Backtrack
    _ -> iprove p (lockL f s)
  Just (Right (f, s)) -> case f of
    -- Category 0
    Top -> True
    Bot -> iprove p s
    -- Category 1
    Var q -> iprove p (lockR f $ subst False q Bot s)
    a :| b -> iprove p (addR a $ addR b s)
    -- Category 2
    a :& b -> all (iprove p) [addR a s, addR b s]
    -- Category 3
    Neg a | res <- iprove p (addL a $ delR s), nullR s || res -> res
    a :> b | res <- iprove p (addL a $ setR b s), nullR s || res -> res
    -- Backtrack
    _ -> iprove p (lockR f s)
  Nothing -> False
