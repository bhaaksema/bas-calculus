module Prover (sprove, iprove, cprove) where

import           Embed   (Axiom, embed)
import           Formula
import qualified Sequent as S

data Logic = Int | Cl deriving Eq

-- | Prove a superintuitionistic theorem
sprove :: [Axiom] -> Formula -> Bool
sprove ax = iprove . embed ax

-- | Prove a intuitionistic theorem (m-G4ip)
iprove :: Formula -> Bool
iprove = prove Int . S.singletonRight . simply

-- | Prove a classical theorem (G3cp)
cprove :: Formula -> Bool
cprove = prove Cl . S.singletonRight . simply

-- | Check the sequent is provable depending on the logic
prove :: Logic -> S.Sequent -> Bool
prove _ [] = False
prove l (e : x) = case e of
  -- Initial sequents
  (0, Left Bot) -> True; (0, Right Top) -> True
  -- Replacement rules
  (1, Right Bot) -> prove l x
  (1, Left (Var s)) -> prove l (map (S.substi s Top) x)
  (1, Left (Var s :> Bot)) -> prove l (map (S.substi s Bot) x)
  -- Unary premise rules
  (2, Right (a :| b))
    -> prove l (S.right a $ S.right b x)
  (2, Right (a :> b)) | l == Cl || null (S.rights x)
    -> prove l (S.left a $ S.right b x)
  (2, Left (a :& b))
    -> prove l (S.left a $ S.left b x)
  (2, Left ((c :& d) :> b))
    -> prove l (S.left (c :> d :> b) x)
  (2, Left ((c :| d) :> b))
    -> prove l (S.left (c :> b) $ S.left (d :> b) x)
  -- Binary premise rules
  (3, Right (a :& b))
    -> all (prove l) [S.right a x, S.right b x]
  (3, Left (a :| b))
    -> all (prove l) [S.left a x, S.left b $ S.right a x]
  (3, Left (a :> b)) | l == Cl || null (S.rights x)
    -> all (prove l) [S.right a x, S.left b x]
  -- Non-invertible rules
  (4, Right (a :> b))
    | prove l (S.left a $ S.setRight b x) -> True
  (4, Left ((c :> d) :> b))
    | prove l (S.left c $ S.left (d :> b) $ S.setRight d x)
    -> prove l (S.left b x)
  -- Continue or fail
  (p, a) -> (p < 5) && prove l (S.insert (succ p, a) x)
