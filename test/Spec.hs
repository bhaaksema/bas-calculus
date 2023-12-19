{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main (main) where

import Data.List (singleton)
import Formula   (Formula (..), neg)
import Prover    (prove)

test :: [(Formula, Bool)]
test = map (\(f, expect) -> (f, prove f == expect))
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
  where
    a:b:c:_ = map (Var . singleton) ['a' .. 'z']

main :: IO ()
main = do
  mapM_ (\(f, b) -> putStrLn $ (if b then "✅ " else "⛔ ") ++ show f) test
  if all snd test then putStrLn "All tests passed!" else error "Some tests failed!"
