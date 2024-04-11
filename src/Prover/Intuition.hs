module Prover.Intuition (iprove, prove) where

import           Formula
import qualified Prover.Classic as C
import           Sequent

-- | Prove a intuitionistic theorem
iprove :: Formula -> Bool
iprove = (\f -> prove f (singleton f)) . simply

-- | Scheduling of signed formulas
schedule :: (Sign, Formula) -> Lock
schedule (L, _ :| _)        = L1
schedule (R, _ :& _)        = L1
schedule (R, Neg _)         = L1
schedule (R, _ :> _)        = L1
schedule (L, Neg (Neg _))   = L1
schedule (L, Neg (_ :> _))  = L1
schedule (L, Neg (_ :& _))  = L2
schedule (L, Neg _ :> _)    = L2
schedule (L, (_ :> _) :> _) = L2
schedule _                  = LOCK

-- | Check provability
prove :: Formula -> Sequent -> Bool
prove p s1 = let (i, h, s) = view s1 in case i of
  LOCK -> False -- Search exhausted
  INIT -> case h of
    -- Initial sequents
    (L, Bot) -> True; (R, Top) -> True
    -- Replacement rules
    (L, Top) -> prove p s
    (R, Bot) -> if nullR s then C.prove (unlock s) else prove p s
    (L, Var q) -> prove p (subst True q Top s)
    (L, Neg (Var q)) -> prove p (subst True q Bot s)
    (R, Var q) -> prove p (lock h $ subst False q Bot s)
    -- Unary premise rules
    (L, a :& b) -> prove p (addL a $ addL b s)
    (L, Neg (a :| b)) -> prove p (addL (Neg a) $ addL (Neg b) s)
    (L, (a :& b) :> c) -> prove p (addL (a :> b :> c) s)
    (L, (a :| b) :> c) -> let q = fresh p in
      prove q (addL (a :> q) $ addL (b :> q) $ addL (q :> c) s)
    (R, a :| b) -> prove p (addR a $ addR b s)
    -- Scheduling
    _ -> prove p (push (schedule h) h s)
  _ -> case h of
    -- Binary premise rules
    (L, Neg (Neg a)) | prove p (addL a $ delR $ unlock s) -> True
    (L, Neg (a :> b)) | prove p (addL a $ addL (Neg b) $ delR $ unlock s) -> True
    (L, a :| b) -> all (prove p) [addL a s, addL b s]
    (R, a :& b) -> all (prove p) [addR a s, addR b s]
    (R, Neg a) | prove p (addL a $ delR s) -> True
    (R, a :> b) | prove p (addL a $ setR b s) -> True
    -- Ternary premise rules
    (L, Neg (a :& b)) | all (prove p) [addL (Neg a) $ delR $ unlock s, addL (Neg b) $ delR $ unlock s] -> True
    (L, Neg a :> b) | all (prove p) [addL a $ delR $ unlock s, addL b $ delR $ unlock s] -> True
    (L, (a :> b) :> c) | q <- fresh p, prove q (addL a $ addL (b :> q) $ addL (q :> c) $ setR q $ unlock s)
      -> all (\pr -> pr (addL c $ unlock s)) [C.prove, prove p]
    _ -> prove p (lock h s)
