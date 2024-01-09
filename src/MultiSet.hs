module MultiSet where

import qualified Data.List as L
import qualified Data.Set  as S
import           Formula   (Formula (..))

data MultiSet = M {
  unVar :: S.Set String, unF :: Bool, unT :: Bool, unFor :: [Formula]
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
pop f m = (\x -> (x, delete x m)) <$> L.find f (unFor m)

delete :: Formula -> MultiSet -> MultiSet
delete (Var v) m = m { unVar = S.delete v $ unVar m }
delete F m       = m { unF = False }
delete T m       = m { unT = False }
delete a m       = m { unFor = L.delete a $ unFor m }

insert :: Formula -> MultiSet -> MultiSet
insert (Var v) m = m { unVar = S.insert v $ unVar m }
insert F m       = m { unF = True }
insert T m       = m { unT = True }
insert a m       = m { unFor = a : unFor m }

(>.) :: Formula -> MultiSet -> MultiSet
(>.) = insert
infixr 8 >.
