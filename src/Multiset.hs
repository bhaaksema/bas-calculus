module Multiset where

import Data.List (uncons)

import Formula (Formula (..))

-- | A finite multiset of formulas
data Multiset = Multiset {
  bot  :: Bool,
  top  :: Bool,
  bin1 :: [Formula],
  bin2 :: [Formula]
} deriving (Eq, Show)

-- | Empty multiset
empty :: Multiset
empty = Multiset False False [] []

-- | Singleton multiset
singleton :: Formula -> Multiset
singleton a = insert a empty

-- | Insert a formula into the multiset
insert :: Formula -> Multiset -> Multiset
insert Bot m = m { bot = True }
insert Top m = m { top = True }
insert a m   = m { bin1 = a : bin1 m }

-- | Convert a multiset to a list of formulas
toList :: Multiset -> [Formula]
toList m = [Bot | bot m] ++ [Top | top m] ++ bin1 m ++ bin2 m

-- | Pop formula at stack pointer position
pop :: Multiset -> Maybe (Formula, Multiset)
pop m = (\(a, as) -> (a, m { bin1 = as })) <$> uncons (bin1 m)

-- | Move the stack pointer down by one
down :: Multiset -> Maybe Multiset
down m | Just (a, as) <- uncons (bin1 m) = Just m { bin1 = as, bin2 = a : bin2 m }
down _ = Nothing

-- | Move the stack pointer completely up
ceil :: Multiset -> Multiset
ceil m = m { bin1 = bin1 m ++ bin2 m, bin2 = [] }
