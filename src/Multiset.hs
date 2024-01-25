module Multiset where

import qualified Data.List as L
import qualified Data.Set  as S

import Formula

-- | A finite multiset of formulas
data Multiset = Multiset {
  var :: S.Set String,
  bot :: Bool,
  top :: Bool,
  con :: [(Formula, Formula)],
  dis :: [(Formula, Formula)],
  imp :: [(Formula, Formula)]
} deriving (Eq, Show)

-- | Empty multiset
empty :: Multiset
empty = Multiset S.empty False False [] [] []

-- | Singleton multiset
singleton :: Formula -> Multiset
singleton a = insert a empty

-- | Insert a formula into the multiset
insert :: Formula -> Multiset -> Multiset
insert (Var s) m  = m { var = S.insert s $ var m }
insert Bot m      = m { bot = True }
insert Top m      = m { top = True }
insert (a :& b) m = m { con = (a, b) : con m }
insert (a :| b) m = m { dis = (a, b) : dis m }
insert (a :> b) m = m { imp = (a, b) : imp m }

-- | Variable substitution on all formulas
contents :: Multiset -> [Formula]
contents m = map Var (S.toList (var m))
  ++ [Bot | bot m]
  ++ [Top | top m]
  ++ map (uncurry (:&)) (con m)
  ++ map (uncurry (:|)) (dis m)
  ++ map (uncurry (:>)) (imp m)

-- | Get a conjunction from the multiset
cget :: Multiset -> Maybe (Formula, Formula, Multiset)
cget m = (\((a, b), cs) -> (a, b, m { con = cs })) <$> L.uncons (con m)

-- | Get a disjunction from the multiset
dget :: Multiset -> Maybe (Formula, Formula, Multiset)
dget m = (\((a, b), ds) -> (a, b, m { dis = ds })) <$> L.uncons (dis m)

-- | Get an implication from the multiset
iget :: Multiset -> Maybe (Formula, Formula, Multiset)
iget = ifind (const True)

-- | Find an implication from the multiset that satisfies a predicate
ifind :: ((Formula, Formula) -> Bool) -> Multiset -> Maybe (Formula, Formula, Multiset)
ifind p m = (\c@(a, b) -> (a, b, idel c m)) <$> L.find p (imp m)

-- | Delete an implication from the multiset
idel :: (Formula, Formula) -> Multiset -> Multiset
idel a m = m { imp = L.delete a $ imp m }
