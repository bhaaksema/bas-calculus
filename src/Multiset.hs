module Multiset where

import qualified Data.List as L
import qualified Data.Set  as S
import           Formula   (Formula (..))

-- | A finite multiset of formulas
data Multiset = M {
  unV :: S.Set String,
  unF :: Bool,
  unC :: [(Formula, Formula)],
  unD :: [(Formula, Formula)],
  unI :: [(Formula, Formula)]
} deriving Eq

-- | Empty multiset
empty :: Multiset
empty = M S.empty False [] [] []

-- | Singleton multiset
singleton :: Formula -> Multiset
singleton a = a +> empty

-- | Check if a variable is in the multiset
vmember :: Formula -> Multiset -> Bool
vmember (V s) = S.member s . unV
vmember _     = const False

-- | Check if two multisets share a variable
vshare :: Multiset -> Multiset -> Bool
vshare x y = not $ S.null (unV x `S.intersection` unV y)

-- | Get a conjunction from the multiset
cget :: Multiset -> Maybe (Formula, Formula, Multiset)
cget m = (\((a, b), cs) -> (a, b, m { unC = cs })) <$> L.uncons (unC m)

-- | Get a disjunction from the multiset
dget :: Multiset -> Maybe (Formula, Formula, Multiset)
dget m = (\((a, b), ds) -> (a, b, m { unD = ds })) <$> L.uncons (unD m)

-- | Get an implication from the multiset
iget :: Multiset -> Maybe (Formula, Formula, Multiset)
iget = ifind (const True)

-- | Find an implication from the multiset that satisfies a predicate
ifind :: ((Formula, Formula) -> Bool) -> Multiset -> Maybe (Formula, Formula, Multiset)
ifind p m = (\x@(a, b) -> (a, b, idel x m)) <$> L.find p (unI m)

-- | Delete an implication from the multiset
idel :: (Formula, Formula) -> Multiset -> Multiset
idel a m = m { unI = L.delete a $ unI m }

-- | Insert a formula into the multiset
insert :: Formula -> Multiset -> Multiset
insert (V s) m    = m { unV = S.insert s $ unV m }
insert F m        = m { unF = True }
insert (a :& b) m = m { unC = (a, b) : unC m }
insert (a :| b) m = m { unD = (a, b) : unD m }
insert (a :> b) m = m { unI = (a, b) : unI m }

-- | Infixed version of 'insert'
(+>) :: Formula -> Multiset -> Multiset
(+>) = insert
infixr 5 +>
