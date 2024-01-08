module Prover.G3cp where

import Data.Bifunctor (first, second)
import Formula        (Formula (..))
import Prover.Utils   (M, Sequent, Stash, (!?), (<:), (>:))

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
rule f ((a :& b) : x, y) = prove1 $ a <: b <: f (x, y)
rule f (x, (a :| b) : y) = prove1 $ a >: b >: f (x, y)
rule f (x, (a :> b) : y) = prove1 $ a <: b >: f (x, y)
rule f (x, (a :& b) : y) = prove1 (a >: z) && prove1 (b >: z) where z = f (x, y)
rule f ((a :| b) : x, y) = prove1 (a <: z) && prove1 (b <: z) where z = f (x, y)
rule f ((a :> b) : x, y) = prove1 (a >: z) && prove1 (b <: z) where z = f (x, y)
rule f (a : x, y)        = rule (first (a :) . f) (x, y)
rule f (_, a : y)        = rule (second (a :) . f) ([], y)
rule _ _                 = False
