{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Bifunctor   (first)
import Data.Text        (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Formula
import Parser  (parse)
import Prover

-- | Unparsed formulas and their status for Cl and Int
problems :: [(Text, (Bool, Bool))]
problems =
  [ ("$false", (False, False))
  , ("$true", (True, True))
  -- Hilbert axioms
  , ("p => p", (True, True))
  , ("p => q => p", (True, True))
  , ("(p => q => r) => (p => q) => (p => r)", (True, True))
  , ("(p => p => q) => (p => q)", (True, True))
  , ("(p => q => r) => (q => p => r)", (True, True))
  , ("$false => p", (True, True))
  , ("(p => q) => (r => p) => (r => q)", (True, True))
  , ("(~p => ~q) => (q => p)", (True, False))
  , ("~~p => p", (True, False))
  , ("~p | p", (True, False))
  , ("(p => ~p) => ~p", (True, True))
  , ("~p => (p => q)", (True, True))
  , ("(p => q) => (p => ~q) => ~p", (True, True))
  -- Conjunction axioms
  , ("p => q => p & q", (True, True))
  , ("p & q => p", (True, True))
  , ("p & q => q", (True, True))
  -- Disjunction axioms
  , ("p => p | q", (True, True))
  , ("q => p | q", (True, True))
  , ("(p => r) => (q => r) => (p | q => r)", (True, True))
  -- Principia Mathematica 2.15
  , ("(~p => q) <=> (~q => p)", (True, False))
  -- Principia Mathematica 2.85
  , ("((p | q) => (p | r)) => (p | (q => r))", (True, False))
  -- Proof Theory and Algebra in Logic 1.8
  , ("~p | ~q => ~(p & q)", (True, True))
  -- Proof Theory and Algebra in Logic 1.13
  , ("~~(p | ~p)", (True, True))
  ]

-- | Parsed version of problems
formulas :: [(Formula, (Bool, Bool))]
formulas = map (first (parse "Spec.hs")) problems

-- | List of logics to test
logics :: [(String, Formula -> Bool, (Bool, Bool) -> Bool)]
logics =
  [ ("Fast Classical Logic", cprove, fst)
  , ("Intuitionistic Logic", iprove, snd)
  , ("Slow Classical Logic", sprove [parse "Spec.hs" "~p | p"], fst)
  ]

-- | Unit tests
tests :: TestTree
tests = testGroup "Unit Tests" $
  map (\(logic, prover, choose)
    -> testGroup logic $
      map (\(formula, expected)
        -> testCase (show formula) $
        prover formula @?= choose expected)
      formulas)
  logics

main :: IO ()
main = defaultMain tests
