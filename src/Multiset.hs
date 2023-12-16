{-# LANGUAGE FlexibleInstances #-}
module Multiset (Multiset (..), defaultMultiset, has, MultisetAddable (..)) where

import Formula (Formula (..))

-- | Represents a multiset of formulas.
data Multiset = Multiset
  { bots :: [Formula]
  , tops :: [Formula]
  , vars :: [String]
  , ands :: [(Formula, Formula)]
  , ors  :: [(Formula, Formula)]
  , imps :: [(Formula, Formula)]
  }

-- | The default empty multiset.
defaultMultiset :: Multiset
defaultMultiset = Multiset [] [] [] [] [] []

-- | Checks if a given function applied to a multiset is non-empty.
has :: (Multiset -> [a]) -> Multiset -> Bool
has f = not . null . f

-- | Typeclass for types that can be added to a multiset.
class MultisetAddable a where
  add :: a -> Multiset -> Multiset

instance MultisetAddable Formula where
  -- | Adds a formula to a multiset.
  add (Var v) s   = s {vars = v : vars s}
  add F s         = s {bots = F : bots s}
  add T s         = s {tops = T : tops s}
  add (And f g) s = s {ands = (f, g) : ands s}
  add (Or f g) s  = s {ors = (f, g) : ors s}
  add (Imp f g) s = s {imps = (f, g) : imps s}

instance MultisetAddable [Formula] where
  -- | Adds a list of formulas to a multiset.
  add [] s     = s
  add (f:fs) s = add fs $ add f s
