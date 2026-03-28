module BSTTests where

import Test.HUnit
import Prelude hiding (lookup)

import BST


main :: IO ()
main = do
    runTestTT allTests
    return ()


allTests :: Test
allTests = TestList [
    TestCase (assertEqual "lookup on an empty tree returns Nothing"
        Nothing
        (lookup 1 (empty :: BST Int String)))
  ]
