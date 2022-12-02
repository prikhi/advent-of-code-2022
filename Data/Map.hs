{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Map
    ( Map
    , empty
    , fromList
    , toList
    , mapKeys
    , foldrWithKeys
    , insert
    , upsert
    , lookup
    )
    where

import Data.List (foldl')

import Prelude hiding (lookup)


-- TODO: balance on construction, re-balance on modification
-- https://www.cmi.ac.in/~madhavan/courses/prog2-2012/lectures/balanced-search-trees-in-haskell.txt

-- | Key-Value Stores via Binary Search Trees.
data Map a b
    = Branch !(BranchData a b)
    | Leaf
    deriving (Show, Read, Eq, Ord)

data BranchData a b =
    BranchData
        { bdLeft :: !(Map a b)
        , bdKey :: !a
        , bdVal :: !b
        , bdRight :: !(Map a b)
        } deriving (Show, Read, Eq, Ord)

instance Functor (Map a) where
    fmap f = \case
        Leaf ->
            Leaf
        Branch bd@BranchData{..} ->
            Branch bd
                { bdLeft = fmap f bdLeft
                , bdRight = fmap f bdRight
                , bdVal = f bdVal
                }

instance Foldable (Map a) where
    foldr reducer acc = \case
        Leaf ->
            acc
        Branch BranchData{..} ->
            let rAcc = foldr reducer acc bdRight
                mAcc = reducer bdVal rAcc
                lAcc = foldr reducer mAcc bdLeft
            in  lAcc



-- | An empty Map
empty :: Map a b
empty = Leaf

-- | Build a Map from a list of (key, value) pairs
fromList :: Ord a => [(a, b)] -> Map a b
fromList = foldl' (\acc (k, v) -> insert k v acc) empty

-- | Map to Lis conversion
toList :: Map a b -> [(a, b)]
toList = \case
    Leaf -> []
    Branch BranchData {..} -> concat [toList bdLeft, [(bdKey, bdVal)], toList bdRight]

-- | Modify all keys of a map
mapKeys :: (a -> c) -> Map a b -> Map c b
mapKeys f = \case
    Leaf ->
        Leaf
    Branch bd@BranchData{..} ->
        Branch bd
            { bdLeft = mapKeys f bdLeft
            , bdRight = mapKeys f bdRight
            , bdKey = f bdKey
            }

foldrWithKeys :: (k -> v -> b -> b) -> b -> Map k v -> b
foldrWithKeys reducer acc = \case
    Leaf ->
        acc
    Branch BranchData{..} ->
        let rAcc = foldrWithKeys reducer acc bdRight
            mAcc = reducer bdKey bdVal rAcc
            lAcc = foldrWithKeys reducer mAcc bdLeft
        in  lAcc

-- | Insert the value at the given key, overriding any existing value
insert :: Ord a => a -> b -> Map a b -> Map a b
insert !k !v = \case
    Leaf ->
        Branch $ BranchData Leaf k v Leaf
    Branch bd@BranchData{..} ->
        case compare k bdKey of
            EQ -> Branch bd { bdVal = v }
            LT -> Branch bd { bdLeft = insert k v bdLeft }
            GT -> Branch bd { bdRight = insert k v bdRight }

upsert :: Ord a => (Maybe b -> b) -> a -> Map a b -> Map a b
upsert maker k = \case
    Leaf ->
        Branch $ BranchData Leaf k (maker Nothing) Leaf
    Branch bd@BranchData{..} ->
        case compare k bdKey of
            EQ -> Branch bd { bdVal = maker $ Just bdVal }
            LT -> Branch bd { bdLeft = upsert maker k bdLeft }
            GT -> Branch bd { bdRight = upsert maker k bdRight }


-- | Query the Map for the given key
lookup :: Ord a => a -> Map a b -> Maybe b
lookup k = \case
    Leaf -> Nothing
    Branch BranchData{..} ->
        case compare k bdKey of
            EQ -> Just bdVal
            LT -> lookup k bdLeft
            GT -> lookup k bdRight
