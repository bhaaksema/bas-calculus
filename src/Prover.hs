{-# LANGUAGE LambdaCase #-}
module Prover (sprove, iprove, cprove) where

import qualified Multiset as M
import qualified Sequent  as S

import Embed   (Axiom, embed)
import Formula (Formula (..))
import Sequent ((+<), (+>))

-- | Prove a superintuitionistic theorem
sprove :: [Axiom] -> Formula -> Bool
sprove ax = iprove . embed ax

-- | Prove a intuitionistic theorem (m-G4ip)
iprove :: Formula -> Bool
iprove = prove True . S.singletonR

-- | Prove a classical theorem (G3cp)
cprove :: Formula -> Bool
cprove = prove False . S.singletonR

-- | Check the sequent is provable depending on the logic
prove :: Bool -> S.Sequent -> Bool
prove i s
  -- Initial sequent
  | M.bot (S.x s) || M.top (S.y s) = True
  -- Glivenko's optimisation
  | i, S.y s == M.singleton Bot = prove False s
  -- Right disjunction
  | Just (a, b, s1) <- S.rDis s = prove i (a +> b +> s1)
  -- Left conjunction
  | Just (a, b, s1) <- S.lCon s = prove i (a +< b +< s1)
  -- Right implication (invertible)
  | Just (a, b, s1) <- S.rImp s, not i || S.y s == M.empty = prove i (a +< b +> s1)
  -- Left implication (invertible)
  | not i, Just (a, b, s1) <- S.lImp s = prove i (a +> s1) && prove i (b +< s1)
  | Just (c :& d, b, s1) <- S.lInvImp s = prove i (c :> (d :> b) +< s1)
  | Just (c :| d, b, s1) <- S.lInvImp s = prove i (c :> b +< d :> b +< s1)
  -- Right conjunction
  | Just (a, b, s1) <- S.rCon s = prove i (a +> s1) && prove i (b +> s1)
  -- Left disjunction (Weich's optimisation)
  | Just (a, b, s1) <- S.lDis s = prove i (a +< s1) && prove i (b +< a +> s1)
  -- Right implication (non-invertible)
  | Just _ <- S.rFindImp (\case(a, b) -> prove i (a +< b +> s { S.y = M.empty })) s = True
  -- Left implication (non-invertible)
  | Just (_, b, s1) <- S.lFindImp (\case
    e@(c :> d, b) -> prove i (c +< d :> b +< d +> s { S.x = M.idel e (S.x s), S.y = M.empty })
    _ -> False) s = prove i (b +< s1)
  -- Failed to prove
  | otherwise = False
