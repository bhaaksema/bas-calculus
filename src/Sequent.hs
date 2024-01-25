{-# LANGUAGE LambdaCase #-}
module Sequent where

import qualified Data.Map as M

import Formula
import Multiset

-- | Multi succedent sequent
data Sequent = Sequent {
  f :: M.Map String Formula,
  x :: Multiset,
  y :: Multiset
}

-- | Create a sequent from a formula
singletonR :: Formula -> Sequent
singletonR a = Sequent M.empty empty (singleton $ simplify a)

apply :: Sequent -> Sequent
apply s = let
  s0 = s { x = empty, y = empty }
  s1 = foldr ((+<) . alter (f s)) s0 (contents (x s))
  in foldr ((+>) . alter (f s)) s1 (contents (y s))

insertL :: Formula -> Sequent -> Sequent
insertL (Var str) s        = apply s { f = M.insert str Top (f s), x = insert Top (x s) }
insertL (Var str :> Bot) s = apply s { f = M.insert str Top (f s), x = insert Bot (x s) }
insertL a s                = s { x = insert (alter (f s) a) (x s) }

(+<) :: Formula -> Sequent -> Sequent
(+<) = insertL
infixr 5 +<

insertR :: Formula -> Sequent -> Sequent
insertR a s = s { y = insert (alter (f s) a) (y s) }

(+>) :: Formula -> Sequent -> Sequent
(+>) = insertR
infixr 5 +>

lCon :: Sequent -> Maybe (Formula, Formula, Sequent)
lCon s = (\(a, b, x1) -> (a, b, s { x = x1 })) <$> cget (x s)

lDis :: Sequent -> Maybe (Formula, Formula, Sequent)
lDis s = (\(a, b, x1) -> (a, b, s { x = x1 })) <$> dget (x s)

lImp :: Sequent -> Maybe (Formula, Formula, Sequent)
lImp s = (\(a, b, x1) -> (a, b, s { x = x1 })) <$> iget (x s)

rCon :: Sequent -> Maybe (Formula, Formula, Sequent)
rCon s = (\(a, b, y1) -> (a, b, s { y = y1 })) <$> cget (y s)

rDis :: Sequent -> Maybe (Formula, Formula, Sequent)
rDis s = (\(a, b, y1) -> (a, b, s { y = y1 })) <$> dget (y s)

rImp :: Sequent -> Maybe (Formula, Formula, Sequent)
rImp s = (\(a, b, y1) -> (a, b, s { y = y1 })) <$> iget (y s)

lFindImp :: ((Formula, Formula) -> Bool) -> Sequent -> Maybe (Formula, Formula, Sequent)
lFindImp p s = (\(a, b, x1) -> (a, b, s { x = x1 })) <$> ifind p (x s)

lInvImp :: Sequent -> Maybe (Formula, Formula, Sequent)
lInvImp = lFindImp (\case (_ :& _, _) -> True; (_ :| _, _) -> True; _ -> False)

rFindImp :: ((Formula, Formula) -> Bool) -> Sequent -> Maybe (Formula, Formula, Sequent)
rFindImp p s = (\(a, b, y1) -> (a, b, s { y = y1 })) <$> ifind p (y s)
