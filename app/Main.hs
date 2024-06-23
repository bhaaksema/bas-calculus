module Main where

import Control.Exception  (SomeException, catch)
import System.Environment (getArgs)

import           Data.Formula
import qualified Parser           as P
import qualified Prover.Classic   as C
import qualified Prover.Intuition as I
import qualified Prover.Super     as S

-- Local parser for the main module
parse :: String -> Formula
parse = P.parse "Main.hs"

-- Checks validity based on the given logic
run :: IO ()
run = do
  [logic, fileName] <- getArgs
  putStrLn fileName
  let kl = [parse "~p | ~~p"]
  let gl = parse "(p => q) | ((p => q) => p)" : kl
  prove <- case logic of
    "cl"  -> return C.prove
    "il"  -> return I.prove
    "kl"  -> return $ S.proveWith S.VAR kl
    "gl"  -> return $ S.proveWith S.VAR gl
    axiom -> return $ S.prove [parse axiom]
  file <- readFile fileName
  let formula = P.parse fileName file
  putStrLn $ if prove formula then "Valid" else "Invalid"

-- Exception handler
handler :: SomeException -> IO ()
handler _ = putStrLn "Usage: super LOGIC FILE\n\
  \\n  LOGIC:\n\
  \    cl\t\tClassical Logic\n\
  \    il\t\tIntuitionistic Logic\n\
  \    kl\t\tJankov Logic\n\
  \    gl\t\tGödel-Dummett Logic\n\
  \    \"FORMULA\"\tAxiomatisation over IL\n\
  \\n  FORMULA:\n\
  \    p | ~A | A&B | A|B | A=>B\n\
  \\n  FILE:\n\
  \    https://tptp.org/TPTP/SyntaxBNF.html"

main :: IO ()
main = catch run handler
