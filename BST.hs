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


-- Render every entry in the BST as a string in ascending key order.
-- Cycle 8: performs an in-order traversal (left subtree, this node,
-- right subtree). Because of the BST invariant this naturally yields
-- the entries in ascending order of key. Each entry is rendered as
-- "<key>: <item>\n".
displayEntries :: (Show key, Show item) => BST key item -> String
displayEntries Leaf = ""
displayEntries (InternalNode currentKey currentItem leftChild rightChild)
    = displayEntries leftChild
      ++ show currentKey ++ ": " ++ show currentItem ++ "\n"
      ++ displayEntries rightChild


-- Remove the entry with the given key, returning the resulting tree.
-- If the key is not present, the tree is returned unchanged.
-- Cycle 11: when the key being removed is NOT at the current node, we
-- recurse into the appropriate subtree based on comparison with the
-- current key. A matching node is still replaced by a Leaf (leaf-
-- removal case from Cycle 10). The partial-child / two-children match
-- cases will be driven by subsequent cycles.
remove :: Ord key => key -> BST key item -> BST key item
remove keyToRemove Leaf = Leaf
remove keyToRemove (InternalNode currentKey currentItem leftChild rightChild)
    = if keyToRemove < currentKey
         then InternalNode currentKey currentItem
                           (remove keyToRemove leftChild)
                           rightChild
         else if keyToRemove > currentKey
                  then InternalNode currentKey currentItem
                                    leftChild
                                    (remove keyToRemove rightChild)
                  else removeCurrent leftChild rightChild


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
