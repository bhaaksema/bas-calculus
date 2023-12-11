module Main (main) where

import Formula

main :: IO ()
main = do
  let f = (neg (T `iff` F) `And` T `And` F) `Or` (Var "p" `And` Var "q")
  print f
  putStr "\nvars:\n"
  mapM_ print $ vars f

  putStr "\nsubformulas:\n"
  mapM_ print $ formulas f
  print $ length $ formulas f

  putStr "\nsubformula fusions:\n"
  mapM_ print $ take 8 $ fusions f
  print $ length $ fusions f
