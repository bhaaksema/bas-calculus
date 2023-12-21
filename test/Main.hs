{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main (main) where

import Data.List (singleton)
import Formula   (Formula (..), neg)
import Prover    (prove0)

intTest :: [(Formula, Bool)]
intTest = map (\(f, expect) -> (f, prove0 [] f == expect))
  [ (a :> a, True)
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
  , ((neg a :> neg b) :> (b :> a), False)
  ] ++ [
    (neg (neg (a :| neg a)), True) -- Ono exercise 1.13
  , ((neg a:| neg b) :> neg (a :& b), True) -- Ono example 1.8
  ] where
  a:b:c:_ = map (Var . singleton) ['a' .. 'z']

main :: IO ()
main = do
  mapM_ (\(f, res) -> putStrLn $ (if res then "✅ " else "⛔ ") ++ show f) intTest
  if all snd intTest then putStrLn "All tests passed!" else error "Some tests failed!"
