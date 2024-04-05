{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.List        (sort)
import           Data.Text        (isInfixOf)
import qualified Data.Text.IO     as T
import           System.Directory
import           Test.Tasty
import           Test.Tasty.HUnit

import Parser (parseFile)
import Prover

-- | Create a problem test case
testProblem :: FilePath -> IO TestTree
testProblem problem = do
  text <- T.readFile problem
  let expect = not ("Non-Theorem" `isInfixOf` text)
  let formula = parseFile problem text
  return (testCase problem $ iprove formula @?= expect)

-- | Create a domain test tree containing problems
testDomain :: FilePath -> IO TestTree
testDomain domain = withCurrentDirectory domain $ do
  problemNames <- listDirectory "."
  problems <- mapM testProblem (sort problemNames)
  return (testGroup domain problems)

-- | Create an ILTP test tree containing domains
testILTP :: FilePath -> IO TestTree
testILTP path = withCurrentDirectory path $ do
  domainNames <- listDirectory "."
  domains <- mapM testDomain (sort domainNames)
  return (testGroup "ILTP" domains)

-- | Run ILTP problems as tasty tests
main :: IO ()
main = do
  testTree <- testILTP "test/problems"
  defaultMain testTree
