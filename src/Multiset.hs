module Multiset where

import qualified Data.List as L
import qualified Data.Set  as S
import           Formula   (Formula (..))

-- | A finite multiset of formulas
data Multiset = Multiset {
  var  :: S.Set String,
  bot  :: Bool,
  top  :: Bool,
  bin1 :: [Formula],
  bin2 :: [Formula]
} deriving Eq

-- | Empty multiset
empty :: Multiset
empty = Multiset S.empty False False [] []

-- | Singleton multiset
singleton :: Formula -> Multiset
singleton a = insert a empty

-- | Insert a formula into the multiset
insert :: Formula -> Multiset -> Multiset
insert (Var s) m = m { var = S.insert s $ var m }
insert Bot m     = m { bot = True }
insert Top m     = m { top = True }
insert a m       = m { bin1 = a : bin1 m }

-- | Infixed version of 'insert'
(+>) :: Formula -> Multiset -> Multiset
(+>) = insert
infixr 5 +>

-- | Check if an atomic variable is in the multiset
vmember :: String -> Multiset -> Bool
vmember s = S.member s . var

-- | Check if two multisets share an atomic variable
vshare :: Multiset -> Multiset -> Bool
vshare x y = not $ S.null (var x `S.intersection` var y)

-- | Pop formula at stack pointer position
bpop :: Multiset -> Maybe (Formula, Multiset)
bpop m = (\(a, as) -> (a, m { bin1 = as })) <$> L.uncons (bin1 m)

-- | Move the stack pointer down by one
bdown :: Multiset -> Multiset
bdown m = m { bin1 = tail (bin1 m), bin2 = head (bin1 m) : bin2 m }

-- | Move the stack pointer completely up
bceil :: Multiset -> Multiset
bceil m = m { bin1 = bin2 m ++ bin1 m, bin2 = [] }
