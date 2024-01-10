module MultiSet where

import qualified Data.List as L
import qualified Data.Set  as S
import           Formula   (Formula (..))

data MultiSet = M {
  unVar :: S.Set String, unF :: Bool, unT :: Bool, unBin :: [Formula]
}

empty :: MultiSet
empty = M S.empty False False []

singleton :: Formula -> MultiSet
singleton a = a >. empty

vmember :: String -> MultiSet -> Bool
vmember v = S.member v . unVar

vshare :: MultiSet -> MultiSet -> Bool
vshare x y = not $ null $ unVar x `S.intersection` unVar y

pop :: (Formula -> Bool) -> MultiSet -> Maybe (Formula, MultiSet)
pop f m = (\x -> (x, m { unBin = L.delete x (unBin m) })) <$> L.find f (unBin m)

(<.) :: (Formula -> Bool) -> MultiSet -> Maybe (Formula, MultiSet)
(<.) = pop
infix 9 <.

insert :: Formula -> MultiSet -> MultiSet
insert (V s) m = m { unVar = S.insert s $ unVar m }
insert F m     = m { unF = True }
insert T m     = m { unT = True }
insert a m     = m { unBin = a : unBin m }

(>.) :: Formula -> MultiSet -> MultiSet
(>.) = insert
infixr 8 >.
