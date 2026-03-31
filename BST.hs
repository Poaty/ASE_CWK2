module BST where

import Prelude hiding (lookup)


-- A binary search tree.
-- A tree is either a Leaf (no entries), or an InternalNode holding a key
-- together with its associated item, plus a left subtree and a right
-- subtree. By the BST invariant, every key in the left subtree is
-- strictly less than the InternalNode's key, and every key in the right
-- subtree is strictly greater.
data BST key item = Leaf
                  | InternalNode key item (BST key item) (BST key item)


-- Construct an empty BST containing no entries.
empty :: BST key item
empty = Leaf


-- Insert a (key, item) entry into the BST.
-- Cycle 2 minimum: every insert produces a fresh single-node tree,
-- discarding any existing structure. Later cycles will force this to
-- preserve and compare with existing entries.
insert :: key -> item -> BST key item -> BST key item
insert newKey newItem oldTree = InternalNode newKey newItem Leaf Leaf


-- Look up a key in the BST.
-- Returns Just the associated item if the key exists, Nothing otherwise.
-- Cycle 2 minimum: a non-empty tree always returns its root's item,
-- ignoring whether the sought key actually matches. Cycle 3 will force
-- the key-comparison logic.
lookup :: key -> BST key item -> Maybe item
lookup soughtKey Leaf = Nothing
lookup soughtKey (InternalNode currentKey currentItem leftChild rightChild)
    = Just currentItem
