module Sequent where

import qualified Data.Set as S

import Formula

-- | Priority for signed formula
data Prio = P0 | P1 | P2 | P3 | P4 | P5
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Sequent is a set of prioritised signed formulae
type Sequent = S.Set (Prio, Sign Formula)

-- | \(O(\log n)\). Retrieve the signed formula with first priority
view :: Sequent -> Maybe (Sign Formula, Sequent)
view x | Just ((i, a), y) <- S.minView x, i < maxBound = Just (a, y)
view _ = Nothing

-- | \(O(\log n)\). Insert a signed formula
(<|) :: (Prio, Sign Formula) -> Sequent -> Sequent
(<|) = S.insert
infixr 4 <|

-- | \(O(n)\). Check if the sequent contains no F-signed formulae
nullFs :: Sequent -> Bool
nullFs x = S.null $ S.filter (isF . snd) x

-- | \(O(n)\). Remove all F-signed formulae
delFs :: Sequent -> Sequent
delFs = S.filter (isT . snd)
