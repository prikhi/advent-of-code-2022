{-# LANGUAGE ViewPatterns #-}

module Day12 where

import Control.Monad
import Control.Monad.ST (ST, runST)
import Data.Array (Array, STArray)
import Data.Char
import Data.Maybe
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.Array as A
import qualified Data.Set as S


main :: IO ()
main =
    getInputAndSolve
        (parseInputRaw parseInputGrid)
        findShortestFromStart
        findShortestFromLows


-- SOLVE

findShortestFromStart :: Array (Int, Int) Char -> Int
findShortestFromStart heightMap =
    either (error "End is unreachable!") id
        . shortestPath heightMap
        . fromMaybe (error "Could not find start!")
        . listToMaybe
        $ findChars heightMap (== 'S')


findShortestFromLows :: Array (Int, Int) Char -> Int
findShortestFromLows heightMap =
    let lows = map fst . filter ((`elem` ['a', 'S']) . snd) $ A.assocs heightMap
     in search (lows, maxBound)
  where
    search :: ([(Int, Int)], Int) -> Int
    search (toSearch, minFound) = case toSearch of
        [] -> minFound
        next : rest ->
            case shortestPath heightMap next of
                Left (S.fromList -> unreachables) ->
                    search (filter (not . (`S.contains` unreachables)) rest, minFound)
                Right pathLength ->
                    search (rest, min minFound pathLength)


-- HELPERS

findChars :: Array (Int, Int) Char -> (Char -> Bool) -> [(Int, Int)]
findChars heightMap test =
    map fst . filter (test . snd) $ A.assocs heightMap


-- TODO: Make this configurable!
--
-- BFSConfig
--      start :: Array -> (Int, Int)
--      destination :: Array -> (Int, Int)
--      initialDistances :: Array (Int, Int) Int
--      isValidNeighbor :: (Int, Int) -> (Int, Int) -> Bool
--      nodeDistance :: Array -> (Int, Int) -> Int
--      getNeighbors :: Array -> (Int, Int) -> [(Int, Int)]
--      isReachable :: STArray -> (Int, Int) -> Bool
--
-- TODO: Implement Priority Queue for finding next unvisited!
shortestPath :: Array (Int, Int) Char -> (Int, Int) -> Either [(Int, Int)] Int
shortestPath heightMap start = runST $ do
    visited <- A.thawSTArray initialVisited
    distances <- A.thawSTArray initialDistances
    recurse visited distances start
  where
    -- Our target location
    destination :: (Int, Int)
    destination =
        fromMaybe (error "Could not find destination!")
            . listToMaybe
            $ findChars heightMap (== 'E')
    -- Initially, we've visited no nodes
    initialVisited :: Array (Int, Int) Bool
    initialVisited =
        A.amap (const False) heightMap
    -- Initial cost of each node is the Int's maxbound.'
    initialDistances :: Array (Int, Int) Int
    initialDistances =
        A.set [(start, 0)] $ A.amap (const maxBound) heightMap
    -- We can move up one height or down many heights
    isValidMove :: (Int, Int) -> (Int, Int) -> Bool
    isValidMove from to =
        let cleanHeight c
                | c == 'E' = fromEnum 'z'
                | c == 'S' = fromEnum 'a'
                | otherwise = fromEnum c
            fromHeight = cleanHeight $ heightMap A.! from
            toHeight = cleanHeight $ heightMap A.! to
         in fromHeight + 1 >= toHeight
    -- Dijkstra! Returns the shortest length to the target or a list of
    -- visited indexes if the target is unreachable.
    recurse
        :: STArray s (Int, Int) Bool
        -- Have we visited the point
        -> STArray s (Int, Int) Int
        -- Whats the path length of the point
        -> (Int, Int)
        -- The next point to process
        -> ST s (Either [(Int, Int)] Int)
    recurse visited distances p = do
        -- Grab valid, unvisited neighbors.
        neighbors <-
            filterM (\ix -> (isValidMove p ix &&) <$> (not <$> A.readSTArray visited ix)) $
                A.getGridNeighborsCardinal heightMap p
        -- Path length so far
        distanceToP <- A.readSTArray distances p
        forM_ neighbors $ \neighbor -> do
            -- If first time seeing a neighbor, set it's path length.
            -- Otherwise, only set it if lower than previously seen
            -- lengths.
            d <- A.readSTArray distances neighbor
            A.writeSTArray distances neighbor $
                if d == maxBound
                    then distanceToP + 1
                    else min d (distanceToP + 1)
        -- Mark the current point as visited
        A.writeSTArray visited p True
        -- Find the next point to check by searching for an unvisited node
        -- with the lowest path length.
        --
        -- Exclude any with a path length of maxBound since we don't know
        -- if they are reachable.
        minUnvisitedDistance <-
            A.freezeSTArray distances
                >>= foldM
                    ( \mbMinPos (pos, dist) -> do
                        notVisited <- not <$> A.readSTArray visited pos
                        case mbMinPos of
                            Nothing -> do
                                if notVisited && dist /= maxBound
                                    then return $ Just (pos, dist)
                                    else return Nothing
                            m@(Just (_, minDist)) ->
                                return $
                                    if dist < minDist && notVisited && dist /= maxBound
                                        then Just (pos, dist)
                                        else m
                    )
                    Nothing
                    . A.assocs
        -- If we've visited the destination, return it's path length.
        visitedDest <- A.readSTArray visited destination
        if visitedDest
            then Right <$> A.readSTArray distances destination
            else case minUnvisitedDistance of
                -- If we found a different node to visit, recurse on that point.
                -- Otherwise, all reachable points have been explored but there is
                -- no path to the destination.
                Just (nextPos, _) ->
                    recurse visited distances nextPos
                Nothing -> do
                    unreachable <- map fst . filter snd . A.assocs <$> A.freezeSTArray visited
                    return $ Left unreachable


-- PARSE

parseInputGrid :: ReadP (Array (Int, Int) Char)
parseInputGrid = parseCharGrid isAlpha <* newline <* eof
