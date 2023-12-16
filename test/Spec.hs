module Main (main) where

import Formula
import Prover

main :: IO ()
main = do
  let f = Imp (Var "p") (Var "p")
  print f
  print $ prove [] f
