{-# LANGUAGE LambdaCase #-}

-- | A Set that can store multiple copies of it's items.
module Data.MultiSet (
    MultiSet,
    empty,
    insert,
    delete,
    uniqueSize,
    totalSize,
) where

import qualified Data.Map as M


newtype MultiSet a = MultiSet
    { fromMultiSet :: M.Map a Int
    }
    deriving (Show, Read, Eq)


-- | The null MultiSet
empty :: MultiSet a
empty = MultiSet M.empty


-- | Insert an element into the MultiSet
insert :: Ord a => a -> MultiSet a -> MultiSet a
insert k = MultiSet . M.upsert (maybe 1 succ) k . fromMultiSet


-- | Remove one instance of an element from the MultiSet
delete :: Ord a => a -> MultiSet a -> MultiSet a
delete k =
    MultiSet
        . M.upsert
            ( \case
                Nothing -> 0
                Just 0 -> 0
                Just x -> pred x
            )
            k
        . fromMultiSet


-- | Count the number of unique items in the MultiSet.
uniqueSize :: MultiSet a -> Int
uniqueSize = length . filter ((/= 0) . snd) . M.toList . fromMultiSet


-- | Count the total number of items(including identical elements) in the
-- MultiSet.
totalSize :: MultiSet a -> Int
totalSize = sum . map snd . M.toList . fromMultiSet
