module BST where

import Prelude hiding (lookup)


-- A binary search tree.
-- For Cycle 1 we only need the empty case (a Leaf, holding no entries).
-- The non-empty case will be introduced when a future test forces it.
data BST key item = Leaf


-- Construct an empty BST containing no entries.
empty :: BST key item
empty = Leaf


-- Look up a key in the BST.
-- Returns Just the associated item if the key exists, Nothing otherwise.
-- Cycle 1 minimum: nothing can be stored yet, so always return Nothing.
lookup :: key -> BST key item -> Maybe item
lookup soughtKey Leaf = Nothing
