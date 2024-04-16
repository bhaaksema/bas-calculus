module Prover.Super (sprove) where

import Embed
import Data.Formula
import Prover.Intuition

-- | Prove a superintuitionistic theorem
sprove :: [Axiom] -> Formula -> Bool
sprove ax = prove . embed ax
