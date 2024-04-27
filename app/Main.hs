{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text          (pack)
import qualified Data.Text.IO       as T
import           System.Environment (getArgs)

import           Parser           (parse)
import qualified Prover.Classic   as C
import qualified Prover.Intuition as I
import qualified Prover.Super     as S

main :: IO ()
main = do
  [logic, fileName] <- getArgs
  putStrLn fileName
  prove <- case logic of
    "cpl" -> return C.prove
    "ipl" -> return I.prove
    "jan" -> return $ S.proveWith S.VAR [parse "Main.hs" "~p | ~~p"]
    axiom -> return $ S.prove [parse "Main.hs" $ pack axiom]
  file <- T.readFile fileName
  let formula = parse fileName file
  putStrLn $ if prove formula then "Valid" else "Invalid"
