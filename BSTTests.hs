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
        (lookup 2 (insert 1 "one" empty) :: Maybe String)),

    TestCase (assertEqual "after smaller-key second insert, root key still retrievable"
        (Just "ten")
        (lookup 10 (insert 5 "five" (insert 10 "ten" empty)))),

    TestCase (assertEqual "after smaller-key second insert, smaller key is retrievable"
        (Just "five")
        (lookup 5 (insert 5 "five" (insert 10 "ten" empty))))
  ]
