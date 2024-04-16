{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO       as T
import           System.Environment (getArgs)

import Parser           (parseFile)
import Prover.Intuition (prove)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  putStrLn fileName
  file <- T.readFile fileName
  let formula = parseFile fileName file
  let result = prove formula
  putStrLn $ if result then "Valid" else "Invalid"
