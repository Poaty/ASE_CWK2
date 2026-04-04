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
-- Cycle 5: a Leaf becomes a fresh single-node tree. Otherwise the new
-- key is compared with the current node's key and the entry is recursed
-- into the appropriate subtree (left for smaller, right for larger).
-- The equal-key case is left for Cycle 6 to drive.
insert :: Ord key => key -> item -> BST key item -> BST key item
insert newKey newItem Leaf = InternalNode newKey newItem Leaf Leaf
insert newKey newItem (InternalNode currentKey currentItem leftChild rightChild)
    = if newKey < currentKey
         then InternalNode currentKey currentItem
                           (insert newKey newItem leftChild)
                           rightChild
         else if newKey > currentKey
                  then InternalNode currentKey currentItem
                                    leftChild
                                    (insert newKey newItem rightChild)
                  else InternalNode newKey newItem leftChild rightChild


-- Look up a key in the BST.
-- Returns Just the associated item if the key exists, Nothing otherwise.
-- Cycle 5: recurses into the appropriate subtree based on the comparison
-- between the sought key and the current node's key.
lookup :: Ord key => key -> BST key item -> Maybe item
lookup soughtKey Leaf = Nothing
lookup soughtKey (InternalNode currentKey currentItem leftChild rightChild)
    = if soughtKey < currentKey
         then lookup soughtKey leftChild
         else if soughtKey > currentKey
                  then lookup soughtKey rightChild
                  else Just currentItem
