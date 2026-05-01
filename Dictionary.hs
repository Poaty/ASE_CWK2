module Dictionary
    ( Dictionary
    , emptyDict
    , insertDict
    , lookupDict
    , displayDict
    , removeDict
    ) where

import Prelude hiding (lookup)
import BST


-- Dictionary constructor isnt exported, so callers cant pattern-match
-- on the underlying tree. lets us swap the implementation later without
-- breaking anything that already uses this module
data Dictionary key item = Dictionary (BST key item)


emptyDict :: Dictionary key item
emptyDict = Dictionary empty


insertDict :: Ord key => key -> item -> Dictionary key item -> Dictionary key item
insertDict newKey newItem (Dictionary theTree)
    = Dictionary (insert newKey newItem theTree)


lookupDict :: Ord key => key -> Dictionary key item -> Maybe item
lookupDict soughtKey (Dictionary theTree) = lookup soughtKey theTree


displayDict :: (Show key, Show item) => Dictionary key item -> String
displayDict (Dictionary theTree) = displayEntries theTree


removeDict :: Ord key => key -> Dictionary key item -> Dictionary key item
removeDict keyToRemove (Dictionary theTree)
    = Dictionary (remove keyToRemove theTree)
