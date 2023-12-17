module Main (main) where

import Formula (Formula(..), neg)
import Prover  (prove)

(==>) :: Formula -> Formula -> Formula
(==>) = Imp

tests :: [(Formula, Bool)]
tests = [
    (a ==> a, True)
  , (a ==> (b ==> a), True)
  , ((a ==> (a ==> b)) ==> (a ==> b), True)
  , ((a ==> (b ==> c)) ==> (b ==> (a ==> c)), True)
  , (F ==> a, True)
  , ((a ==> b) ==> ((c ==> a) ==> (c ==> b)), True)
  , ((a ==> c) ==> ((b ==> c) ==> ((a `Or` b) ==> c)), True)
  , (a ==> (a `Or` b), True)
  , (b ==> (a `Or` b), True)
  , ((c ==> a) ==> ((c ==> b) ==> (c ==> (a `And` b))), True)
  , ((a `And` b) ==> a, True)
  , ((a `And` b) ==> b, True)
  , (neg (neg a) ==> a, False)
  , (neg a `Or` a, False)
  , ((a ==> (b ==> c)) ==> ((a ==> b) ==> (a ==> c)), True)
  ]
  where
    a = Var "a"
    b = Var "b"
    c = Var "c"

main :: IO ()
main = mapM_ (\(f, b) -> putStrLn $ (if prove [] f == b then "✅ " else "⛔ ") ++ show f) tests
