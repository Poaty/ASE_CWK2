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
        (lookup 1 (empty :: BST Int String))),

    TestCase (assertEqual "lookup after a single insert returns Just the item"
        (Just "one")
        (lookup 1 (insert 1 "one" empty))),

    TestCase (assertEqual "lookup of an absent key in a single-node tree returns Nothing"
        Nothing
        (lookup 2 (insert 1 "one" empty) :: Maybe String))
  ]
