{-# LANGUAGE OverloadedStrings #-}
module Main where

import Formula (Formula)
import Parser  (parse)
import Prover  (iprove)

msg :: Either a Formula -> String
msg (Right f) = show f ++ '\n' : show (iprove f)
msg (Left _)  = "No well-formed formula"

main :: IO ()
main = do
  let a = parse "Main.hs" "$false => p"
  putStrLn $ msg a
