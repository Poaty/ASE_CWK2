module BST where

import Prelude hiding (lookup)


-- A binary search tree. All keys in the left subtree are strictly less
-- than the node's key; all keys in the right subtree are strictly greater.
data BST key item = Leaf
                  | InternalNode key item (BST key item) (BST key item)
    deriving (Eq, Show)


empty :: BST key item
empty = Leaf


insert :: Ord key => key -> item -> BST key item -> BST key item
insert newKey newItem Leaf = InternalNode newKey newItem Leaf Leaf
insert newKey newItem (InternalNode currentKey currentItem leftChild rightChild)
    | newKey < currentKey =
        let newLeftChild = insert newKey newItem leftChild
         in InternalNode currentKey currentItem newLeftChild rightChild
    | newKey > currentKey =
        let newRightChild = insert newKey newItem rightChild
         in InternalNode currentKey currentItem leftChild newRightChild
    | otherwise =
        InternalNode newKey newItem leftChild rightChild


lookup :: Ord key => key -> BST key item -> Maybe item
lookup soughtKey Leaf = Nothing
lookup soughtKey (InternalNode currentKey currentItem leftChild rightChild)
    | soughtKey < currentKey = lookup soughtKey leftChild
    | soughtKey > currentKey = lookup soughtKey rightChild
    | otherwise              = Just currentItem


-- In-order traversal, so entries appear in ascending key order.
displayEntries :: (Show key, Show item) => BST key item -> String
displayEntries Leaf = ""
displayEntries (InternalNode currentKey currentItem leftChild rightChild)
    = displayEntries leftChild
      ++ show currentKey ++ ": " ++ show currentItem ++ "\n"
      ++ displayEntries rightChild


remove :: Ord key => key -> BST key item -> BST key item
remove keyToRemove Leaf = Leaf
remove keyToRemove (InternalNode currentKey currentItem leftChild rightChild)
    | keyToRemove < currentKey =
        let newLeftChild = remove keyToRemove leftChild
         in InternalNode currentKey currentItem newLeftChild rightChild
    | keyToRemove > currentKey =
        let newRightChild = remove keyToRemove rightChild
         in InternalNode currentKey currentItem leftChild newRightChild
    | otherwise =
        removeCurrent leftChild rightChild


-- Stitch the two subtrees of a removed node back together. When both
-- children are non-empty, the in-order successor takes the removed
-- node's place.
removeCurrent :: BST key item -> BST key item -> BST key item
removeCurrent Leaf rightChild = rightChild
removeCurrent leftChild Leaf  = leftChild
removeCurrent leftChild rightChild
    = let (successorKey, successorItem, rightWithoutMin) = popMin rightChild
       in InternalNode successorKey successorItem leftChild rightWithoutMin


-- Precondition: input is non-empty.
popMin :: BST key item -> (key, item, BST key item)
popMin (InternalNode currentKey currentItem Leaf rightChild)
    = (currentKey, currentItem, rightChild)
popMin (InternalNode currentKey currentItem leftChild rightChild)
    = let (minKey, minItem, leftWithoutMin) = popMin leftChild
       in (minKey, minItem, InternalNode currentKey currentItem leftWithoutMin rightChild)


-- Returns the tree unchanged if there is no left child to promote.
rotateRight :: BST key item -> BST key item
rotateRight (InternalNode parentKey parentItem
                          (InternalNode childKey childItem childLeft childRight)
                          parentRight)
    = let newRightSubtree = InternalNode parentKey parentItem childRight parentRight
       in InternalNode childKey childItem childLeft newRightSubtree
rotateRight anyOtherTree = anyOtherTree


-- Mirror of rotateRight.
rotateLeft :: BST key item -> BST key item
rotateLeft (InternalNode parentKey parentItem
                         parentLeft
                         (InternalNode childKey childItem childLeft childRight))
    = let newLeftSubtree = InternalNode parentKey parentItem parentLeft childLeft
       in InternalNode childKey childItem newLeftSubtree childRight
rotateLeft anyOtherTree = anyOtherTree


countMatching :: (key -> item -> Bool) -> BST key item -> Int
countMatching predicate Leaf = 0
countMatching predicate (InternalNode currentKey currentItem leftChild rightChild)
    = let countInLeft  = countMatching predicate leftChild
          countInRight = countMatching predicate rightChild
          currentMatch = if predicate currentKey currentItem then 1 else 0
       in currentMatch + countInLeft + countInRight
