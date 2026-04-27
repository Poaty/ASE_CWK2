module Dictionary (Dictionary, emptyDict, lookupDict) where

import Prelude hiding (lookup)
import BST


-- An abstract dictionary backed internally by a binary search tree.
-- The export list above hides the Dictionary data constructor, so
-- callers cannot pattern match on or directly construct values of this
-- type; they can only build dictionaries through the public functions
-- below.
data Dictionary key item = Dictionary (BST key item)


-- The empty dictionary.
emptyDict :: Dictionary key item
emptyDict = Dictionary empty


-- Look up a key in the dictionary.
-- Returns Just the associated item if the key exists, Nothing otherwise.
-- Cycle 22 minimum: every lookup returns Nothing. A future cycle will
-- force this to consult the underlying tree.
lookupDict :: key -> Dictionary key item -> Maybe item
lookupDict soughtKey anyDictionary = Nothing
