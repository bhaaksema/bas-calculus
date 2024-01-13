module MultiSet where

import qualified Data.List as L
import qualified Data.Set  as S
import           Formula   (Formula (..))

data MultiSet = M {
  unV :: S.Set String, unF :: Bool, unT :: Bool,
  unC :: [(Formula, Formula)],
  unD :: [(Formula, Formula)],
  unI :: [(Formula, Formula)]
}

empty :: MultiSet
empty = M S.empty False False [] [] []

singleton :: Formula -> MultiSet
singleton a = a +> empty

vmember :: String -> MultiSet -> Bool
vmember v = S.member v . unV

vshare :: MultiSet -> MultiSet -> Bool
vshare x y = not $ null $ unV x `S.intersection` unV y

cget :: MultiSet -> Maybe (Formula, Formula, MultiSet)
cget m = (\((a, b), cs) -> (a, b, m { unC = cs })) <$> L.uncons (unC m)

dget :: MultiSet -> Maybe (Formula, Formula, MultiSet)
dget m = (\((a, b), ds) -> (a, b, m { unD = ds })) <$> L.uncons (unD m)

iget :: MultiSet -> Maybe (Formula, Formula, MultiSet)
iget = ifind (const True)

ifind :: ((Formula, Formula) -> Bool) -> MultiSet -> Maybe (Formula, Formula, MultiSet)
ifind f m = (\x@(a, b) -> (a, b, idel x m)) <$> L.find f (unI m)

idel :: (Formula, Formula) -> MultiSet -> MultiSet
idel a m = m { unI = L.delete a $ unI m }

insert :: Formula -> MultiSet -> MultiSet
insert (V s) m    = m { unV = S.insert s $ unV m }
insert F m        = m { unF = True }
insert T m        = m { unT = True }
insert (a :& b) m = m { unC = (a, b) : unC m }
insert (a :| b) m = m { unD = (a, b) : unD m }
insert (a :> b) m = m { unI = (a, b) : unI m }

(+>) :: Formula -> MultiSet -> MultiSet
(+>) = insert
infixr 8 +>
