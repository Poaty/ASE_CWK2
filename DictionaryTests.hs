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


-- ---------------------------------------------------------------------
-- Test runner
-- ---------------------------------------------------------------------

main :: IO ()
main = do
    quickCheck prop_lookupDict_onEmpty_isNothing
