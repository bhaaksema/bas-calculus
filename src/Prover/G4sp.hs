module Prover.G4sp where

import           Bounding    (for)
import           Formula     (Formula)
import qualified Multiset    as M
import qualified Prover.G4ip as I

-- | Prove a superintuitionistic theorem
prove :: [Formula] -> Formula -> Bool
prove as f = I.prove1 (foldr M.insert M.empty (for as f), f)

-- prove as ([x], y) = I.prove1 (for as (x :> f)), f)
-- prove as (xs, y) = I.prove1 (unions (map (for as) (f : xs)), f)
