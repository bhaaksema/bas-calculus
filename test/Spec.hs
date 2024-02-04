module Main (main) where

import Control.Monad (unless)

import Formula
import Prover

a :: Formula; b :: Formula; c :: Formula
(a, b, c) = (Var "a", Var "b", Var "c")

itests :: [(Formula, Bool)]
itests =
  [ (Bot, False)
  , (Top, True)
  , (a :> a, True)
  , (a :> b :> a, True) -- left-weakening axiom
  , ((a :> a :> b) :> a :> b, True) -- contraction axiom
  , ((a :> b :> c) :> b :> a :> c, True) -- exchange axiom
  , (Bot :> a, True)
  , ((a :> b) :> (c :> a) :> c :> b, True)
  , (neg (neg a) :> a, False) -- law of double negation
  , (neg a :| a, False) -- law of excluded middle
  -- disjunction axioms
  , ((a :> c) :> (b :> c) :> a :| b :> c, True)
  , (a :> (a :| b), True)
  , (b :> (a :| b), True)
  -- conjunction axioms
  , ((c :> a) :> (c :> b) :> c :> a :& b, True)
  , (a :& b :> a, True)
  , (a :& b :> b, True)
  -- Łukasiewicz's axioms
  , ((a :> b :> c) :> (a :> b) :> a :> c, True)
  , (neg a :> neg b <:> b :> a, False)
  , (neg (neg (a :| neg a)), True) -- Ono exercise 1.13
  , (neg a :| neg b :> neg (a :& b), True) -- Ono example 1.8
  ]

check :: (Formula -> Bool) -> [(Formula, Bool)] -> [(Formula, Bool)]
check p = map (\(f, e) -> (f, p f == e))

main :: IO ()
main = do
  let ctests = head itests : map (\(x, _) -> (x, True)) (tail itests)
  let rci = check cprove ctests ++ check iprove itests
  let rs1 = check (sprove [neg a :| a]) ((neg (neg b) :> b, True) : ctests)
  let rs2 = check (sprove [neg (neg b) :> b]) ((a :| neg a, True) : ctests)
  let res = rci ++ rs1 ++ rs2
  let resi = zipWith (\(f, r) i -> (show i ++ ' ' : show f, r)) res [(1 :: Int)..]
  mapM_ (\(f, r) -> do putStr f; putStrLn (if r then " ✅" else undefined " ⛔")) resi
  unless (all snd res) undefined
