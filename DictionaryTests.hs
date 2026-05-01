module DictionaryTests where

import Test.QuickCheck
import Data.List (isInfixOf)

import Dictionary


prop_lookupDict_onEmpty_isNothing :: Int -> Bool
prop_lookupDict_onEmpty_isNothing soughtKey =
    (lookupDict soughtKey emptyDict :: Maybe String) == Nothing


prop_lookupDict_afterInsert_returnsJust :: Int -> String -> Bool
prop_lookupDict_afterInsert_returnsJust newKey newItem =
    lookupDict newKey (insertDict newKey newItem emptyDict) == Just newItem


prop_displayDict_singletonContainsKey :: Int -> String -> Bool
prop_displayDict_singletonContainsKey newKey newItem =
    show newKey `isInfixOf` displayDict (insertDict newKey newItem emptyDict)


prop_removeDict_afterInsert_isNothing :: Int -> String -> Bool
prop_removeDict_afterInsert_isNothing keyToRemove newItem =
    lookupDict keyToRemove
               (removeDict keyToRemove (insertDict keyToRemove newItem emptyDict))
        == Nothing


main :: IO ()
main = do
    quickCheck prop_lookupDict_onEmpty_isNothing
    quickCheck prop_lookupDict_afterInsert_returnsJust
    quickCheck prop_displayDict_singletonContainsKey
    quickCheck prop_removeDict_afterInsert_isNothing
