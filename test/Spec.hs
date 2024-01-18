module Main (main) where

import           Formula      (Formula (..), iff, neg)
import qualified Prover.G3cp  as C
import qualified Prover.G4ipm as I
import qualified Prover.G4sp  as S

a :: Formula; b :: Formula; c :: Formula
(a, b, c) = (V "a", V "b", V "c")

itests :: [(Formula, Bool)]
itests =
  [ (F, False)
  , (a :> a, True)
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
  ]

check :: (Formula -> Bool) -> [(Formula, Bool)] -> [(Formula, Bool)]
check p = map (\(f, e) -> (f, p f == e))

main :: IO ()
main = do
  let ctests = head itests : map (\(x, _) -> (x, True)) (tail itests)
  let r1 = check C.prove ctests
  let r2 = check I.prove itests
  let r3 = check (S.prove [a :| neg a]) ((neg (neg b) :> b, True) : ctests)
  let r4 = check (S.prove [neg (neg b) :> b]) ((a :| neg a, True) : ctests)
  mapM_ (\(f, res) -> putStrLn $ (if res then "✅" else "⛔") ++ " C " ++ show f) r1
  mapM_ (\(f, res) -> putStrLn $ (if res then "✅" else "⛔") ++ " I " ++ show f) r2
  mapM_ (\(f, res) -> putStrLn $ (if res then "✅" else "⛔") ++ " S " ++ show f) r3
  mapM_ (\(f, res) -> putStrLn $ (if res then "✅" else "⛔") ++ " T " ++ show f) r4
  if all snd (r1 ++ r2 ++ r3 ++ r4) then putStrLn "All tests passed!" else error "Some tests failed!"
