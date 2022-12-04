module Data.Array (
    module A,
    set,
    setAll,
    sparseGrid,
    showGrid,
    showGridWith,
    getGridNeighborsCardinal,
    getGridNeighborsDiagonal,
) where

import Data.Function
import GHC.Arr as A

import qualified Data.List as L


-- | Set the values of multiple indexes in bulk.
set :: (A.Ix i) => [(i, a)] -> A.Array i a -> A.Array i a
set is arr =
    A.accum (\_ x -> x) arr is
{-# INLINEABLE set #-}


-- | Set all given indexes to the passed value.
setAll :: (A.Ix i) => a -> [i] -> A.Array i a -> A.Array i a
setAll a is arr =
    A.accum (\_ x -> x) arr (zip is (repeat a))
{-# INLINEABLE setAll #-}


-- GRID HELPERS

-- | Build a grid from a list of points that may not represent the complete
-- grid-space.
sparseGrid :: a -> [((Int, Int), a)] -> A.Array (Int, Int) a
sparseGrid defaultVal points =
    let maxX = maximum $ map (fst . fst) points
        maxY = maximum $ map (snd . fst) points
        defaults = [((x, y), defaultVal) | x <- [0 .. maxX], y <- [0 .. maxY]]
     in array ((0, 0), (maxX, maxY)) $ L.unionBy ((==) `on` fst) points defaults
{-# INLINEABLE sparseGrid #-}


-- | Render a grid line by line using the elements 'show' function.
showGrid :: Show a => A.Array (Int, Int) a -> String
showGrid =
    showGridWith show
{-# INLINEABLE showGrid #-}


-- | Render a grid line by line using the given element-rendering function.
showGridWith :: (a -> String) -> A.Array (Int, Int) a -> String
showGridWith renderer grid =
    let (_, (width, height)) = A.bounds grid
     in unlines
            [ concat [renderer (grid A.! (col, row)) | col <- [0 .. width]]
            | row <- [0 .. height]
            ]
{-# INLINEABLE showGridWith #-}


-- | Get the neighbors in the cardinal directions.
getGridNeighborsCardinal :: A.Array (Int, Int) a -> (Int, Int) -> [(Int, Int)]
getGridNeighborsCardinal grid (x, y) =
    let surroundings =
            [ (x + dx, y + dy)
            | dx <- [-1 .. 1]
            , dy <- [-1 .. 1]
            , not $
                (dy == 0 && dx == 0)
                    || (abs dy == 1 && abs dx == 1)
            ]
        (_, (width, height)) = A.bounds grid
     in filter
            ( \(sx, sy) ->
                sx >= 0 && sx <= width && sy >= 0 && sy <= height
            )
            surroundings
{-# INLINEABLE getGridNeighborsCardinal #-}


-- | Get all neighbors, including the diagonals.
getGridNeighborsDiagonal :: A.Array (Int, Int) a -> (Int, Int) -> [(Int, Int)]
getGridNeighborsDiagonal grid (x, y) =
    let surroundings =
            [ (x + dx, y + dy)
            | dx <- [-1 .. 1]
            , dy <- [-1 .. 1]
            , not (dx == 0 && dy == 0)
            ]
        (_, (width, height)) = A.bounds grid
     in filter
            ( \(sx, sy) ->
                sx >= 0 && sx <= width && sy >= 0 && sy <= height
            )
            surroundings
{-# INLINEABLE getGridNeighborsDiagonal #-}
