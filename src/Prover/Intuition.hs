module Prover.Intuition (prove) where

import           Data.Collection     (Category (..))
import           Data.Formula
import qualified Prover.Classic as Cl
import           Data.Sequent

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
iprove p s1 = case view s1 of
  Just (sgn, f, s) -> case sgn of
    L -> case f of
      -- Category 0
      Bot -> True
      Top -> iprove p s
      -- Category 1
      Var q -> iprove p (subst True q Top s)
      a :& b -> iprove p (add L a $ add L b s)
      Neg (a :| b) -> iprove p (add L (Neg a) $ add L (Neg b) s)
      (a :& b) :> c -> iprove p (add L (a :> b :> c) s)
      (a :| b) :> c -> let q = fresh p in
        iprove q (add L (a :> q) $ add L (b :> q) $ add L (q :> c) s)
      -- Category 2
      a :| b -> all (iprove p) [add L a s, add L b s]
      -- Category 4
      _ :> _ | not $ Cl.prove (toFormula s1) -> False
      Neg a :> b | nullR s ||
        iprove p (add L a $ delR $ unlock s) -> iprove p (add L b $ unlock s)
      (a :> b) :> c | q <- fresh p, nullR s ||
        iprove q (add L a $ add L (b :> q) $ add L (q :> c) $ setR q $ unlock s)
        -> iprove p (add L c $ unlock s)
      -- Category 5
      Neg (Neg a) -> iprove p (add L a $ delR $ unlock s)
      Neg (a :> b) -> iprove p (add L a $ add L (Neg b) $ delR $ unlock s)
      -- Category 6
      Neg (a :& b) -> all (\c -> iprove p (add L (Neg c) $ delR $ unlock s)) [a, b]
      -- Backtrack
      _ -> iprove p (lock L f s)
    R -> case f of
      -- Category 0
      Bot    -> if nullR s then Cl.prove (toFormula s1) else
        iprove p s
      Top    -> True
      -- Category 1
      Var q  -> iprove p (lock R f $ subst False q Bot s)
      a :| b -> iprove p (add R a $ add R b s)
      -- Category 2
      a :& b -> all (iprove p) [add R a s, add R b s]
      -- Category 3
      Neg a | res <- iprove p (add L a $ delR s), nullR s || res -> res
      a :> b | res <- iprove p (add L a $ setR b s), nullR s || res -> res
      -- Backtrack
      _ -> iprove p (lock R f s)
  Nothing -> False
