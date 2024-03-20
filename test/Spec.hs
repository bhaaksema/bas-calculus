{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (unless)
import Data.Either   (fromRight)
import Formula
import Parser        (parse)
import Prover

p, q, r :: Formula
(p, q, r) = (Var "p", Var "q", Var "r")

itests :: [(Formula, Bool)]
itests =
  [ (Bot, False)
  , (Top, True)
  , (p :> p, True)
  , (p :> q :> p, True) -- left-weakening axiom
  , ((p :> p :> q) :> p :> q, True) -- contraction axiom
  , ((p :> q :> r) :> q :> p :> r, True) -- exchange axiom
  , (Bot :> p, True)
  , ((p :> q) :> (r :> p) :> r :> q, True)
  , (neg (neg p) :> p, False) -- law of double negation
  , (neg p :| p, False) -- law of excluded middle
  -- disjunction axioms
  , ((p :> r) :> (q :> r) :> p :| q :> r, True)
  , (p :> (p :| q), True)
  , (q :> (p :| q), True)
  -- conjunction axioms
  , ((r :> p) :> (r :> q) :> r :> p :& q, True)
  , (p :& q :> p, True)
  , (p :& q :> q, True)
  -- Łukasiewicz's axioms
  , ((p :> q :> r) :> (p :> q) :> p :> r, True)
  , (neg p :> neg q <:> q :> p, False)
  -- Ono example 1.8
  , (neg p :| neg q :> neg (p :& q), True)
  -- Ono exercise 1.13
  , (neg (neg (p :| neg p)), True)
  -- Principia Mathematica 2.15
  , ((neg p :> q) <:> (neg q :> p), False)
  -- Principia Mathematica 2.85
  , (((p :| q) :> (p :| r)) :> (p :| (q :> r)), False)
  ]

check :: (Formula -> Bool) -> [(Formula, Bool)] -> [(Formula, Bool)]
check g = map (\(f, e) -> (f, g f == e))

main :: IO ()
main = do
  let ctests = head itests : map (\(x, _) -> (x, True)) (tail itests)
  let res = check cprove ctests
         ++ check iprove itests
         ++ check (sprove [neg p :| p]) ctests
  let idx_res = zip res [(1::Int)..]
  mapM_ (\((f, _), i) -> do putStrLn (show i ++ ' ' : show f)) idx_res
  unless (all (snd . fst) idx_res) undefined
  print (fromRight Bot (parse "p => p"))
