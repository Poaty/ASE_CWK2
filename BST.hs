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
-- A Leaf becomes a fresh single-node tree. Otherwise the new key is
-- compared with the current node's key: if smaller the entry is
-- recursed into the left subtree, if larger into the right subtree,
-- and if equal the existing entry's item is overwritten.
insert :: Ord key => key -> item -> BST key item -> BST key item
insert newKey newItem Leaf = InternalNode newKey newItem Leaf Leaf
insert newKey newItem (InternalNode currentKey currentItem leftChild rightChild)
    | newKey < currentKey = InternalNode currentKey currentItem
                                         (insert newKey newItem leftChild)
                                         rightChild
    | newKey > currentKey = InternalNode currentKey currentItem
                                         leftChild
                                         (insert newKey newItem rightChild)
    | otherwise           = InternalNode newKey newItem leftChild rightChild


-- Look up a key in the BST.
-- Returns Just the associated item if the key exists, Nothing otherwise.
-- Recurses into the appropriate subtree based on the comparison between
-- the sought key and the current node's key.
lookup :: Ord key => key -> BST key item -> Maybe item
lookup soughtKey Leaf = Nothing
lookup soughtKey (InternalNode currentKey currentItem leftChild rightChild)
    | soughtKey < currentKey = lookup soughtKey leftChild
    | soughtKey > currentKey = lookup soughtKey rightChild
    | otherwise              = Just currentItem


-- Render every entry in the BST as a string in ascending key order.
-- Performs an in-order traversal (left subtree, this node, right
-- subtree). Because of the BST invariant this naturally yields the
-- entries in ascending order of key. Each entry is rendered as
-- "<key>: <item>\n".
displayEntries :: (Show key, Show item) => BST key item -> String
displayEntries Leaf = ""
displayEntries (InternalNode currentKey currentItem leftChild rightChild)
    = displayEntries leftChild
      ++ show currentKey ++ ": " ++ show currentItem ++ "\n"
      ++ displayEntries rightChild


-- Remove the entry with the given key, returning the resulting tree.
-- If the key is not present, the tree is returned unchanged.
-- For a non-matching node we recurse into the appropriate subtree.
-- For the matching node we delegate to removeCurrent, which deals
-- with the four structural cases (leaf, only left, only right, both).
remove :: Ord key => key -> BST key item -> BST key item
remove keyToRemove Leaf = Leaf
remove keyToRemove (InternalNode currentKey currentItem leftChild rightChild)
    | keyToRemove < currentKey = InternalNode currentKey currentItem
                                              (remove keyToRemove leftChild)
                                              rightChild
    | keyToRemove > currentKey = InternalNode currentKey currentItem
                                              leftChild
                                              (remove keyToRemove rightChild)
    | otherwise                = removeCurrent leftChild rightChild


-- Helper: combine the two subtrees of a node that is being removed.
-- If either subtree is a Leaf the other one is promoted (this also
-- handles the leaf-removal case where both are Leaf). When both
-- subtrees are non-empty we replace the removed node with its in-order
-- successor (the smallest-keyed entry in the right subtree), and that
-- entry is removed from the right subtree to avoid duplicating it.
removeCurrent :: BST key item -> BST key item -> BST key item
removeCurrent Leaf rightChild = rightChild
removeCurrent leftChild Leaf  = leftChild
removeCurrent leftChild rightChild
    = let (successorKey, successorItem, rightWithoutMin) = popMin rightChild
       in InternalNode successorKey successorItem leftChild rightWithoutMin


-- Helper: remove the leftmost (smallest-key) entry from a non-empty
-- BST. Returns the popped entry's key, its item, and the tree with that
-- entry removed. Used to find the in-order successor when removing a
-- node that has both children.
-- Precondition: the input tree must be non-empty.
popMin :: BST key item -> (key, item, BST key item)
popMin (InternalNode currentKey currentItem Leaf rightChild)
    = (currentKey, currentItem, rightChild)
popMin (InternalNode currentKey currentItem leftChild rightChild)
    = let (minKey, minItem, leftWithoutMin) = popMin leftChild
       in (minKey, minItem, InternalNode currentKey currentItem leftWithoutMin rightChild)
