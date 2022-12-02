{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Day01 where

import           Control.Arrow                  ( (&&&) )
import           Control.Monad
import           Data.Array                     ( Array )
import           Data.Bifunctor
import           Data.Char
import           Data.Either
import           Data.Function                  ( on )
import           Data.Functor
import           Data.Map                       ( Map )
import           Data.Maybe
import           Data.Set                       ( Set )
import           Text.ParserCombinators.ReadP

import           Harness
import           ParseHelper

import qualified Data.Array                    as A
import qualified Data.List                     as L
import qualified Data.Map                      as M
import qualified Data.Set                      as S

import           Debug.Trace


-- (parseInput lineParser) OR (parseInputRaw fullInputParser)
main :: IO ()
main =
    getInputAndSolve (parseInputRaw elvesParser) highestCalorieElf topThreeElves


-- SOLVE
highestCalorieElf :: [Elf] -> Int
highestCalorieElf = totalCalories . L.maximumBy (compare `on` totalCalories)

topThreeElves :: [Elf] -> Int
topThreeElves = (\(a, b, c) -> a + b + c) . L.foldl' go (0, 0, 0)
  where
    go :: (Int, Int, Int) -> Elf -> (Int, Int, Int)
    go top@(high, mid, low) = \case
        e | totalCalories e >= high -> (totalCalories e, high, mid)
          | totalCalories e >= mid  -> (high, totalCalories e, mid)
          | totalCalories e >= low  -> (high, mid, totalCalories e)
          | otherwise               -> top



-- HELPERS

totalCalories :: Elf -> Int
totalCalories = sum . items

-- PARSE

newtype Elf = Elf
    { items :: [Int]
    } deriving (Show, Read, Eq, Ord)

elvesParser :: ReadP [Elf]
elvesParser =
    sepBy1 (Elf <$> sepBy1 parseInt newline) (newline *> newline)
        <* newline
        <* eof
