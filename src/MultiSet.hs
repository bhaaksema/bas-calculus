module MultiSet where

import           Data.List (uncons)
import qualified Data.Set  as S
import           Formula   (Formula (..))

data MultiSet = M {
  unVar :: S.Set String, unF :: Bool, unT :: Bool,
  unAn  :: [(Formula, Formula)],
  unOr  :: [(Formula, Formula)],
  unIm  :: [(Formula, Formula)]
}

empty :: MultiSet
empty = M S.empty False False [] [] []

singleton :: Formula -> MultiSet
singleton a = a >. empty

shareVar :: MultiSet -> MultiSet -> Bool
shareVar x y = not $ null $ unVar x `S.intersection` unVar y

popAn :: MultiSet -> Maybe (Formula, Formula, MultiSet)
popAn m = (\((a, b), xs) -> (a, b, m {unAn = xs})) <$> uncons (unAn m)

popOr :: MultiSet -> Maybe (Formula, Formula, MultiSet)
popOr m = (\((a, b), xs) -> (a, b, m {unOr = xs})) <$> uncons (unOr m)

popIm :: MultiSet -> Maybe (Formula, Formula, MultiSet)
popIm m = (\((a, b), xs) -> (a, b, m {unIm = xs})) <$> uncons (unIm m)

insert :: Formula -> MultiSet -> MultiSet
insert (Var v) m  = m { unVar = S.insert v $ unVar m }
insert F m        = m { unF = True }
insert T m        = m { unT = True }
insert (a :& b) m = m { unAn = (a, b) : unAn m }
insert (a :| b) m = m { unOr = (a, b) : unOr m }
insert (a :> b) m = m { unIm = (a, b) : unIm m }

(>.) :: Formula -> MultiSet -> MultiSet
(>.) = insert
infixr 8 >.
