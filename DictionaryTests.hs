module DictionaryTests where

-- QuickCheck provides the property-based testing framework used for
-- exercising the encapsulated Dictionary module.
import Test.QuickCheck

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


-- ---------------------------------------------------------------------
-- Test runner
-- ---------------------------------------------------------------------

main :: IO ()
main = do
    quickCheck prop_lookupDict_onEmpty_isNothing
    quickCheck prop_lookupDict_afterInsert_returnsJust
