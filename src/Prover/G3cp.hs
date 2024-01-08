module Prover.G3cp where

import Data.Bifunctor (first, second)
import Formula        (Formula (..))
import Prover.Utils   (Sequent, Stash, (!:), (!?))

-- Multi-succedent
type M = [Formula]

-- Prove a formula
prove :: Formula -> Bool
prove a = prove1 ([], [a])

-- Prove a sequent
prove1 :: Sequent M -> Bool
prove1 s = axiom s || rule id s

-- Check if the sequent satisfies an axiom
axiom :: Sequent M -> Bool
axiom (v@(Var _) : x, y) = v !? y || axiom (x, y)
axiom (F : _, _)         = True
axiom (_, y)             = T !? y

-- Check if the sequent satisfies a rule
rule :: Stash M -> Sequent M -> Bool
rule f ((a :& b) : x, y) = prove1 $ f (a !: b !: x, y)
rule f (x, (a :| b) : y) = prove1 $ f (x, a !: b !: y)
rule f (x, (a :> b) : y) = prove1 $ f (a !: x, b !: y)
rule f (x, (a :& b) : y) = prove1 (f (x, a !: y)) && prove1 (f (x, b !: y))
rule f ((a :| b) : x, y) = prove1 (f (a !: x, y)) && prove1 (f (b !: x, y))
rule f ((a :> b) : x, y) = prove1 (f (x, a !: y)) && prove1 (f (b !: x, y))
rule f (a : x, y)        = rule (first (a !:) . f) (x, y)
rule f (_, a : y)        = rule (second (a !:) . f) ([], y)
rule _ _                 = False
