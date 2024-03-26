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
  file <- T.readFile fileName
  let formula = parse fileName file
  let result = iprove formula
  putStrLn $ show formula ++ '\n' : if
    result then "Theorem" else "Non-Theorem"
