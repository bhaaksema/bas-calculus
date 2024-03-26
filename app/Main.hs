{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO       as T
import           System.Environment (getArgs)

import Parser (parse)
import Prover (iprove)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  putStrLn fileName
  file <- T.readFile fileName
  let formula = parse fileName file
  let result = iprove formula
  putStrLn $ if result then "Valid" else "Invalid"
