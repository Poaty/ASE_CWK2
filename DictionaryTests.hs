module DictionaryTests where

-- QuickCheck provides the property-based testing framework used for
-- exercising the encapsulated Dictionary module.
import Test.QuickCheck
import Data.List (isInfixOf)

import Dictionary


-- ---------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------

-- Property: looking up any key in an empty dictionary yields Nothing,
-- because no entries have been stored yet.
prop_lookupDict_onEmpty_isNothing :: Int -> Bool
prop_lookupDict_onEmpty_isNothing soughtKey =
    (lookupDict soughtKey emptyDict :: Maybe String) == Nothing


-- Property: after inserting a (key, item) entry into the empty
-- dictionary, looking up that same key should return Just the item.
prop_lookupDict_afterInsert_returnsJust :: Int -> String -> Bool
prop_lookupDict_afterInsert_returnsJust newKey newItem =
    lookupDict newKey (insertDict newKey newItem emptyDict) == Just newItem


-- Property: rendering a dictionary that contains one inserted entry
-- should produce a string that mentions the inserted key. (We compare
-- against `show newKey` because that is how the underlying renderer
-- formats the key.)
prop_displayDict_singletonContainsKey :: Int -> String -> Bool
prop_displayDict_singletonContainsKey newKey newItem =
    show newKey `isInfixOf` displayDict (insertDict newKey newItem emptyDict)


-- Property: after inserting an entry and then removing it by key,
-- looking up that key should once again return Nothing.
prop_removeDict_afterInsert_isNothing :: Int -> String -> Bool
prop_removeDict_afterInsert_isNothing keyToRemove newItem =
    lookupDict keyToRemove
               (removeDict keyToRemove (insertDict keyToRemove newItem emptyDict))
        == Nothing


-- ---------------------------------------------------------------------
-- Test runner
-- ---------------------------------------------------------------------

main :: IO ()
main = do
    quickCheck prop_lookupDict_onEmpty_isNothing
    quickCheck prop_lookupDict_afterInsert_returnsJust
    quickCheck prop_displayDict_singletonContainsKey
    quickCheck prop_removeDict_afterInsert_isNothing
