module Prover.Super (sprove) where

import           Embed
import           Formula
import qualified Prover.Intuition as I

-- | Prove a superintuitionistic theorem
sprove :: [Axiom] -> Formula -> Bool
sprove ax = I.iprove . embed ax
