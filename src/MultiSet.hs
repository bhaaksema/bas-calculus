module MultiSet where

import qualified Data.List as L
import qualified Data.Set  as S
import           Formula   (Formula (..))

-- | A finite multiset of formulas
data MultiSet = M {
  unV :: S.Set String, unF :: Bool, unT :: Bool,
  unC :: [(Formula, Formula)],
  unD :: [(Formula, Formula)],
  unI :: [(Formula, Formula)]
}

-- | Empty multiset
empty :: MultiSet
empty = M S.empty False False [] [] []

-- | Singleton multiset
singleton :: Formula -> MultiSet
singleton a = a +> empty

-- | Check if a variable is in the multiset
vmember :: Formula -> MultiSet -> Bool
vmember (V s) = S.member s . unV
vmember _     = const False

-- | Check if two multisets share a variable
vshare :: MultiSet -> MultiSet -> Bool
vshare x y = not $ null $ unV x `S.intersection` unV y

-- | Get a conjunction from the multiset
cget :: MultiSet -> Maybe (Formula, Formula, MultiSet)
cget m = (\((a, b), cs) -> (a, b, m { unC = cs })) <$> L.uncons (unC m)

-- | Get a disjunction from the multiset
dget :: MultiSet -> Maybe (Formula, Formula, MultiSet)
dget m = (\((a, b), ds) -> (a, b, m { unD = ds })) <$> L.uncons (unD m)

-- | Get an implication from the multiset
iget :: MultiSet -> Maybe (Formula, Formula, MultiSet)
iget = ifind (const True)

-- | Get an implication from the multiset that satisfies a predicate
ifind :: ((Formula, Formula) -> Bool) -> MultiSet -> Maybe (Formula, Formula, MultiSet)
ifind f m = (\x@(a, b) -> (a, b, idel x m)) <$> L.find f (unI m)

-- | Delete an implication from the multiset
idel :: (Formula, Formula) -> MultiSet -> MultiSet
idel a m = m { unI = L.delete a $ unI m }

-- | Insert a formula into the multiset
insert :: Formula -> MultiSet -> MultiSet
insert (V s) m    = m { unV = S.insert s $ unV m }
insert F m        = m { unF = True }
insert T m        = m { unT = True }
insert (a :& b) m = m { unC = (a, b) : unC m }
insert (a :| b) m = m { unD = (a, b) : unD m }
insert (a :> b) m = m { unI = (a, b) : unI m }

-- | Infixed version of 'insert'
(+>) :: Formula -> MultiSet -> MultiSet
(+>) = insert
infixr 8 +>
