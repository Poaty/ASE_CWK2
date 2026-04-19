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
        (lookup 5 (insert 5 "five" (insert 10 "ten" empty)))),

    TestCase (assertEqual "after larger-key second insert, root key still retrievable"
        (Just "ten")
        (lookup 10 (insert 20 "twenty" (insert 10 "ten" empty)))),

    TestCase (assertEqual "after larger-key second insert, larger key is retrievable"
        (Just "twenty")
        (lookup 20 (insert 20 "twenty" (insert 10 "ten" empty)))),

    TestCase (assertEqual "re-inserting the same key overwrites the existing item"
        (Just "uno")
        (lookup 1 (insert 1 "uno" (insert 1 "one" empty)))),

    TestCase (assertEqual "displayEntries on an empty tree returns the empty string"
        ""
        (displayEntries (empty :: BST Int String))),

    TestCase (assertEqual "displayEntries renders entries in ascending key order"
        "5: \"five\"\n10: \"ten\"\n15: \"fifteen\"\n"
        (displayEntries (insert 5 "five" (insert 15 "fifteen" (insert 10 "ten" empty))))),

    TestCase (assertEqual "remove on an empty tree leaves it empty"
        Nothing
        (lookup 1 (remove 1 (empty :: BST Int String)))),

    TestCase (assertEqual "remove the only entry from a single-node tree leaves it empty"
        Nothing
        (lookup 1 (remove 1 (insert 1 "one" empty)) :: Maybe String)),

    TestCase (assertEqual "remove of a non-root key removes that entry from the subtree"
        Nothing
        (lookup 5 (remove 5 (insert 5 "five" (insert 10 "ten" empty))))),

    TestCase (assertEqual "remove of a non-root key leaves the other entry in place"
        (Just "ten")
        (lookup 10 (remove 5 (insert 5 "five" (insert 10 "ten" empty))))),

    TestCase (assertEqual "remove root with only a right child: removed key is gone"
        Nothing
        (lookup 10 (remove 10 (insert 20 "twenty" (insert 10 "ten" empty))))),

    TestCase (assertEqual "remove root with only a right child: right subtree survives"
        (Just "twenty")
        (lookup 20 (remove 10 (insert 20 "twenty" (insert 10 "ten" empty))))),

    TestCase (assertEqual "remove root with only a left child: removed key is gone"
        Nothing
        (lookup 10 (remove 10 (insert 5 "five" (insert 10 "ten" empty))))),

    TestCase (assertEqual "remove root with only a left child: left subtree survives"
        (Just "five")
        (lookup 5 (remove 10 (insert 5 "five" (insert 10 "ten" empty))))),

    TestCase (assertEqual "remove root with two children: removed key is gone"
        Nothing
        (lookup 10 (remove 10 (insert 15 "fifteen" (insert 5 "five" (insert 10 "ten" empty)))))),

    TestCase (assertEqual "remove root with two children: left subtree survives"
        (Just "five")
        (lookup 5 (remove 10 (insert 15 "fifteen" (insert 5 "five" (insert 10 "ten" empty)))))),

    TestCase (assertEqual "remove root with two children: right subtree survives"
        (Just "fifteen")
        (lookup 15 (remove 10 (insert 15 "fifteen" (insert 5 "five" (insert 10 "ten" empty)))))),

    TestCase (assertEqual "remove root with two children: remaining entries are still in order"
        "5: \"five\"\n15: \"fifteen\"\n"
        (displayEntries (remove 10 (insert 15 "fifteen" (insert 5 "five" (insert 10 "ten" empty)))))),

    TestCase (assertEqual "countMatching on an empty tree returns 0"
        0
        (countMatching (\_ _ -> True) (empty :: BST Int String))),

    TestCase (assertEqual "countMatching with an always-true predicate returns the total entry count"
        3
        (countMatching (\_ _ -> True)
                       (insert 5 "five" (insert 15 "fifteen" (insert 10 "ten" empty)))))
  ]
