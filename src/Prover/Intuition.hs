module Prover.Intuition (iprove, prove) where

import           Formula
import qualified Prover.Classic as C
import           Sequent

-- | Prove a intuitionistic theorem
iprove :: Formula -> Bool
iprove = (\f -> prove f (singleton f)) . simply

-- | Scheduling of signed formulas
schedule :: (Sign, Formula) -> Category
schedule (L, _ :| _)        = C2
schedule (R, _ :& _)        = C2
schedule (R, Neg _)         = C3
schedule (R, _ :> _)        = C3
schedule (L, Neg _ :> _)    = C4
schedule (L, (_ :> _) :> _) = C4
schedule (L, Neg (Neg _))   = C5
schedule (L, Neg (_ :> _))  = C5
schedule (L, Neg (_ :& _))  = C6
schedule _                  = CX

-- | Check provability
prove :: Formula -> Sequent -> Bool
prove p s1 = let (i, f, s) = view s1 in case i of
  CX -> False -- Search exhausted
  C1 -> case f of
    -- Category 0
    (L, Bot) -> True; (R, Top) -> True
    -- Category 1
    (L, Top) -> prove p s
    (R, Bot) -> if nullR s then C.prove (unlock s) else prove p s
    (L, Var q) -> prove p (subst True q Top s)
    (L, Neg (Var q)) -> prove p (subst True q Bot s)
    (R, Var q) -> prove p (lock f $ subst False q Bot s)
    (L, a :& b) -> prove p (add L a $ add L b s)
    (L, Neg (a :| b)) -> prove p (add L (Neg a) $ add L (Neg b) s)
    (L, (a :& b) :> c) -> prove p (add L (a :> b :> c) s)
    (L, (a :| b) :> c) -> let q = fresh p in
      prove q (add L (a :> q) $ add L (b :> q) $ add L (q :> c) s)
    (R, a :| b) -> prove p (add R a $ add R b s)
    -- Scheduling
    _ -> prove p (push (schedule f) f s)
  _ -> case f of
    -- Category 2
    (L, a :| b) -> all (prove p) [add L a s, add L b s]
    (R, a :& b) -> all (prove p) [add R a s, add R b s]
    -- Category 3
    (R, Neg a) | res <- prove p (add L a $ delR s), nullR s || res -> res
    (R, a :> b) | res <- prove p (add L a $ setR b s), nullR s || res -> res
    -- Category 4
    (L, a :> b) | not $ C.prove (add L (a :> b) $ unlock s) -> False
    (L, Neg a :> b) | nullR s ||
      prove p (add L a $ delR $ unlock s) -> prove p (add L b $ unlock s)
    (L, (a :> b) :> c) | q <- fresh p, nullR s ||
      prove q (add L a $ add L (b :> q) $ add L (q :> c) $ setR q $ unlock s)
      -> prove p (add L c $ unlock s)
    -- Category 5
    (L, Neg (Neg a)) -> prove p (add L a $ delR $ unlock s)
    (L, Neg (a :> b)) -> prove p (add L a $ add L (Neg b) $ delR $ unlock s)
    -- Category 6
    (L, Neg (a :& b)) -> all (\c -> prove p (add L (Neg c) $ delR $ unlock s)) [a, b]
    -- Backtracking
    _ -> prove p (lock f s)
