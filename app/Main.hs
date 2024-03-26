{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text          (isInfixOf)
import Data.Text.IO       (readFile)
import Prelude            hiding (readFile)
import System.Environment (getArgs)

import Parser (parse)
import Prover (iprove)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  file <- readFile fileName
  let expect = not $ "Non-Theorem" `isInfixOf` file
  let formula = parse fileName file
  let result = iprove formula
  putStrLn $ show formula
    ++ '\n' : (if result then "Theorem" else "Non-Theorem")
    ++ ' '  : (if expect == result then "✅" else "⛔")
