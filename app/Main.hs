module Main where

import qualified Formula (Formula(..), neg, iff)

main :: IO ()
main = do
  print $ Formula.And [Formula.neg $ Formula.iff Formula.T Formula.F, Formula.T, Formula.F]
