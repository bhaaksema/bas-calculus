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
  let kc = [parse "~p | ~~p"]
  let lc = parse "(p => q) | ((p => q) => p)" : kc
  prove <- case logic of
    "cl"  -> return C.prove
    "il"  -> return I.prove
    "kc"  -> return $ S.proveWith S.VAR kc
    "lc"  -> return $ S.proveWith S.VAR lc
    axiom -> return $ S.prove [parse axiom]
  file <- readFile fileName
  let formula = P.parse fileName file
  putStrLn $ if prove formula then "Valid" else "Invalid"

-- Exception handler
handler :: SomeException -> IO ()
handler _ = error "Usage: super LOGIC FILE\n\
  \\n  LOGIC:\n\
  \    cl\t\tClassical Propositional Logic (CPL)\n\
  \    il\t\tIntuitionistic Propositional Logic (IPL)\n\
  \    kc\t\tJankov Logic\n\
  \    lc\t\tGödel-Dummett Logic\n\
  \    \"FORMULA\"\tAxiomatisation over IPL\n\
  \\n  FORMULA:\n\
  \    p | ~A | A&B | A|B | A=>B\n\
  \\n  FILE:\n\
  \    https://tptp.org/TPTP/SyntaxBNF.html"

main :: IO ()
main = catch run handler
