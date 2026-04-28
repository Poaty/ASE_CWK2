module Dictionary (Dictionary, emptyDict, insertDict, lookupDict) where

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


-- Insert a (key, item) entry into the dictionary. If the key already
-- exists, its associated item is overwritten. Delegates to BST.insert,
-- preserving the binary-search-tree invariant of the underlying tree.
insertDict :: Ord key => key -> item -> Dictionary key item -> Dictionary key item
insertDict newKey newItem (Dictionary theTree)
    = Dictionary (insert newKey newItem theTree)


-- Look up a key in the dictionary.
-- Returns Just the associated item if the key exists, Nothing otherwise.
-- Delegates to BST.lookup.
lookupDict :: Ord key => key -> Dictionary key item -> Maybe item
lookupDict soughtKey (Dictionary theTree) = lookup soughtKey theTree
