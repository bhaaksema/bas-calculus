module Prover.G4sp where

import           Bounding     (for)
import           Formula      (Formula (..))
import qualified Prover.G4ipm as I

-- | Prove a superintuitionistic theorem
prove :: [Formula] -> Formula -> Bool
prove as f = I.prove $ foldr1 (:&) (for as f) :> f
