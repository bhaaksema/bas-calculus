module Main where

import Control.Exception  (SomeException, catch)
import System.Environment (getArgs)
import System.Exit        (exitSuccess)

import           Data.Formula
import qualified Parser           as P
import qualified Prover.Classic   as C
import qualified Prover.Intuition as I
import qualified Prover.Super     as S

-- Local parser for the main module
parse :: String -> Formula
parse = P.parseTPTP ""

-- Gets the logic and file name from cli
input :: IO (String, String)
input = do
  [logic, fileName] <- getArgs
  return (logic, fileName)

-- Input exception handler
handler :: SomeException -> IO (String, String)
handler _ = do
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

-- Solving based on the given logic
main :: IO ()
main = do
  (logic, fileName) <- catch input handler
  putStr ("Solving " ++ fileName ++ " using ")
  let kl = [parse "~p | ~~p"]
  let gl = parse "(p => q) | ((p => q) => p)" : kl
  prove <- case logic of
    "-cl" -> putStrLn "Classical Logic" >> return C.prove
    "-il" -> putStrLn "Intuitionistic Logic" >> return I.prove
    "-kl" -> putStrLn "Jankov Logic" >> return (S.proveWith S.VAR kl)
    "-gl" -> putStrLn "Gödel-Dummett Logic" >> return (S.proveWith S.VAR gl)
    axiom -> do
      putStrLn ("Intuitionistic Logic + " ++ axiom)
      return (S.prove $ P.parseAxiom axiom)
  file <- readFile fileName
  let formula = P.parseTPTP fileName file
  putStr "SZS status "
  putStrLn (if prove formula then "Theorem" else "CounterSatisfiable")
