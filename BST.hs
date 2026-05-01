module BST where

import Prelude hiding (lookup)


-- BST invariant: left subtree keys < this key < right subtree keys
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


-- in-order traversal so the output is already sorted by key
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


-- two-children case promotes the in-order successor to preserve the BST invariant
removeCurrent :: BST key item -> BST key item -> BST key item
removeCurrent Leaf rightChild = rightChild
removeCurrent leftChild Leaf  = leftChild
removeCurrent leftChild rightChild
    = let (successorKey, successorItem, rightWithoutMin) = popMin rightChild
       in InternalNode successorKey successorItem leftChild rightWithoutMin


-- precondition: non-empty. only called from removeCurrent which guards both children
popMin :: BST key item -> (key, item, BST key item)
popMin (InternalNode currentKey currentItem Leaf rightChild)
    = (currentKey, currentItem, rightChild)
popMin (InternalNode currentKey currentItem leftChild rightChild)
    = let (minKey, minItem, leftWithoutMin) = popMin leftChild
       in (minKey, minItem, InternalNode currentKey currentItem leftWithoutMin rightChild)


-- TODO: not wired into insert/remove — here for a future self-balancing variant
rotateRight :: BST key item -> BST key item
rotateRight (InternalNode parentKey parentItem
                          (InternalNode childKey childItem childLeft childRight)
                          parentRight)
    = let newRightSubtree = InternalNode parentKey parentItem childRight parentRight
       in InternalNode childKey childItem childLeft newRightSubtree
rotateRight anyOtherTree = anyOtherTree


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
