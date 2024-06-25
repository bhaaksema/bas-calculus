module Main where

import Control.Exception  (SomeException, catch)
import System.Environment (getArgs)
import System.Exit        (exitSuccess)

import           Data.Formula
import qualified Parser           as P
import qualified Prover.Classic   as Cl
import qualified Prover.Intuition as Il
import qualified Prover.Super     as Sl

-- Reads the logic and file name from cli
input :: IO (String, String)
input = getArgs >>= \args -> return (head args, head $ tail args)

-- Outputs the SZS success status
output :: Bool -> IO ()
output result = putStr (if result then "Theorem" else "CounterSatisfiable")

-- User input exception handler
inputHandler :: SomeException -> IO (String, String)
inputHandler _ = do
  putStrLn "Usage: super LOGIC FILE\n\
  \\n  LOGIC:\n\
  \    -cl\t\tClassical Logic\n\
  \    -il\t\tIntuitionistic Logic\n\
  \    -kl\t\tJankov Logic\n\
  \    -gl\t\tGödel-Dummett Logic\n\
  \    \"FORMULA\"\tAxiomatisation over IL\n\
  \\n  FORMULA:\n\
  \    p | ~A | A&B | A|B | A=>B\n\
  \\n  FILE:\n\
  \    https://tptp.org/TPTP/SyntaxBNF.html"
  exitSuccess

-- Miscellaneous exception handler
errorHandler :: SomeException -> IO ()
errorHandler _ = putStr "Error"

-- | Solver with SZS based output:
-- https://tptp.org/Seminars/TPTP/Documents/SZSOntology.txt
main :: IO ()
main = do
  (logic, fileName) <- catch input inputHandler
  let kl = P.parseAxiom "~p | ~~p"
  let gl = P.parseAxiom "(p => q) | ((p => q) => p)" ++ kl
  putStr "Solving with "
  prove <- case logic of
    "-cl" -> putStrLn "Classical Logic" >> return Cl.prove
    "-il" -> putStrLn "Intuitionistic Logic" >> return Il.prove
    "-kl" -> putStrLn "Jankov Logic" >> return (Sl.proveWith Sl.VAR kl)
    "-gl" -> putStrLn "Gödel-Dummett Logic" >> return (Sl.proveWith Sl.VAR gl)
    axiom -> do
      putStrLn ("Intuitionistic Logic + " ++ axiom)
      let axioms = P.parseAxiom axiom
      if Sl.prove axioms (Var $ -1)
        then putStrLn "SZS status ContradictoryAxioms" >> exitSuccess
        else return (Sl.prove axioms)
  file <- readFile fileName
  let formula = P.parseTPTP fileName file
  putStr "SZS status "
  catch (output $ prove formula) errorHandler
  putStrLn (" for " ++ fileName)
