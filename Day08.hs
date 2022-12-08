{-# LANGUAGE TupleSections #-}

module Day08 where

import Data.Array (Array)
import Data.Bifunctor
import Data.Foldable

import Harness
import ParseHelper

import qualified Data.Array as A
import qualified Data.List as L


main :: IO ()
main =
    getInputAndSolve
        (parseInputRaw parseIntGrid)
        countEdgeVisibleTrees
        findHighestScenicScore


-- SOLVE

countEdgeVisibleTrees :: Array (Int, Int) Int -> Int
countEdgeVisibleTrees arr =
    let visibleIxs =
            concatMap (findVisibleBy selectColumn) [0 .. maxX]
                <> concatMap (findVisibleBy selectRow) [0 .. maxY]
        visibleGrid = A.setAll True visibleIxs $ A.amap (const False) arr
     in length . filter id $ A.elems visibleGrid
  where
    (_, (maxX, maxY)) = A.bounds arr
    findVisibleBy
        :: (Int -> Array (Int, Int) Int -> [((Int, Int), Int)])
        -> Int
        -> [(Int, Int)]
    findVisibleBy selector ix =
        let trees = selector ix arr
         in findVisible trees <> findVisible (reverse trees)

    findVisible :: [((Int, Int), Int)] -> [(Int, Int)]
    findVisible = snd . foldl' go (Nothing, [])
      where
        go :: (Maybe Int, [(Int, Int)]) -> ((Int, Int), Int) -> (Maybe Int, [(Int, Int)])
        go (mbHighest, visibleIxs) (nextIx, nextHeight) = case mbHighest of
            Nothing ->
                (Just nextHeight, nextIx : visibleIxs)
            Just highest ->
                if nextHeight > highest
                    then (Just nextHeight, nextIx : visibleIxs)
                    else (mbHighest, visibleIxs)


findHighestScenicScore :: Array (Int, Int) Int -> Int
findHighestScenicScore arr =
    maximum [calculateScenicScore ix | ix <- A.indices arr]
  where
    (_, (maxX, maxY)) = A.bounds arr
    rowHeights :: Array Int [Int]
    rowHeights =
        A.listArray
            (0, maxY)
            [map snd $ selectRow y arr | y <- [0 .. maxY]]
    colHeights :: Array Int [Int]
    colHeights =
        A.listArray
            (0, maxX)
            [map snd $ selectColumn x arr | x <- [0 .. maxX]]
    calculateScenicScore :: (Int, Int) -> Int
    calculateScenicScore ix@(x, y) =
        let cellHeight = arr A.! ix
            (left, right) = bimap reverse (drop 1) . L.splitAt x $ rowHeights A.! y
            (above, below) = bimap reverse (drop 1) . L.splitAt y $ colHeights A.! x
            countUnblocked hs =
                let unblocked = takeWhile (< cellHeight) hs
                 in if length unblocked == length hs
                        then length hs
                        else succ $ length unblocked
         in product $ map countUnblocked [left, right, above, below]


-- HELPERS

selectColumn :: Int -> Array (Int, Int) Int -> [((Int, Int), Int)]
selectColumn targetCol arr =
    let (_, (_, maxY)) = A.bounds arr
     in [(ix, arr A.! ix) | ix <- (targetCol,) <$> [0 .. maxY]]


selectRow :: Int -> Array (Int, Int) Int -> [((Int, Int), Int)]
selectRow targetRow arr =
    let (_, (maxX, _)) = A.bounds arr
     in [(ix, arr A.! ix) | ix <- (,targetRow) <$> [0 .. maxX]]

-- PARSE
