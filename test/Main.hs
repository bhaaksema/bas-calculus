{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main (main) where

import Data.List (singleton)
import Formula   (Formula (..), neg)
import Prover    (prove)

intTest :: [(Formula, Bool)]
intTest = map (\(f, expect) -> (f, prove [] f == expect))
  [ (a :> a, True)
  , (a :> (b :> a), True)
  , ((a :> (a :> b)) :> (a :> b), True)
  , ((a :> (b :> c)) :> (b :> (a :> c)), True)
  , (F :> a, True)
  , ((a :> b) :> ((c :> a) :> (c :> b)), True)
  , ((a :> c) :> ((b :> c) :> ((a :| b) :> c)), True)
  , (a :> (a :| b), True)
  , (b :> (a :| b), True)
  , ((c :> a) :> ((c :> b) :> (c :> (a :& b))), True)
  , ((a :& b) :> a, True)
  , ((a :& b) :> b, True)
  , (neg (neg a) :> a, False)
  , (neg a :| a, False)
  , ((a :> (b :> c)) :> ((a :> b) :> (a :> c)), True)
  ]

superTest :: [(Formula, Bool)]
superTest = map (\(axi, f, expect) -> (f, prove axi f == expect))
  [ ([neg (neg b)], a :| neg a, True)
  -- , (neg (neg b), [a :| neg a], True) -- TODO: does not terminate in reasonable time
  ]

a :: Formula
b :: Formula
c :: Formula
a:b:c:_ = map (Var . singleton) ['a' .. 'z']

main :: IO ()
main = do
  mapM_ (\(f, res) -> putStrLn $ (if res then "✅ " else "⛔ ") ++ show f) (intTest ++ superTest)
  if all snd intTest && all snd superTest then putStrLn "All tests passed!" else error "Some tests failed!"
