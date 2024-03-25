{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text             (isInfixOf)
import Data.Text.IO          (readFile)
import Prelude               hiding (readFile)
import System.Environment    (getArgs)
import Text.Megaparsec.Error (errorBundlePretty)

import Parser (parse)
import Prover (iprove)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  file <- readFile fileName
  let expect = not $ "Non-Theorem" `isInfixOf` file
  let parseResult = parse fileName file
  putStrLn $ case parseResult of
    Left e -> errorBundlePretty e
    Right formula | result <- iprove formula
      -> show formula
      ++ '\n' : (if result then "Theorem" else "Non-Theorem")
      ++ ' '  : (if expect == result then "✅" else "⛔")
