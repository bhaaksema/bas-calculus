module Main (main) where

import Formula (Formula (..), iff, neg)
import Prover  (prove)

a :: Formula; b :: Formula; c :: Formula
(a, b, c) = (V "a", V "b", V "c")

tests :: [([Formula], Formula, Bool)]
tests = map (\(f, expect) -> ([], f, expect))
  [ (a :> a, True)
  , (F, False)
  , (a :> (b :> a), True) -- left-weakening axiom
  , ((a :> (a :> b)) :> (a :> b), True) -- contraction axiom
  , ((a :> (b :> c)) :> (b :> (a :> c)), True) -- exchange axiom
  , (F :> a, True)
  , ((a :> b) :> ((c :> a) :> (c :> b)), True)
  , (neg (neg a) :> a, False) -- law of double negation
  , (neg a :| a, False) -- law of excluded middle
  -- disjunction axioms
  , ((a :> c) :> ((b :> c) :> ((a :| b) :> c)), True)
  , (a :> (a :| b), True)
  , (b :> (a :| b), True)
  -- conjunction axioms
  , ((c :> a) :> ((c :> b) :> (c :> (a :& b))), True)
  , ((a :& b) :> a, True)
  , ((a :& b) :> b, True)
  -- Łukasiewicz's axioms
  , ((a :> (b :> c)) :> ((a :> b) :> (a :> c)), True)
  , ((neg a :> neg b) `iff` (b :> a), False)
  , (neg (neg (a :| neg a)), True) -- Ono exercise 1.13
  , ((neg a:| neg b) :> neg (a :& b), True) -- Ono example 1.8
  ] ++
  [ ([a :| neg a], neg (neg b) :> b, True)
  , ([neg (neg b) :> b], a :| neg a, True)
  , ([a :| neg a], F, False)
  ]

main :: IO ()
main = do
  let result = map (\(axi, f, expect) -> (f, prove axi f == expect)) tests
  mapM_ (\(f, res) -> putStrLn $ (if res then "✅ " else "⛔ ") ++ show f) result
  if all snd result then putStrLn "All tests passed!" else error "Some tests failed!"
