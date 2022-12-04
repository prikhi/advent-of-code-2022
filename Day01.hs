module Day01 where

import Data.Function (on)
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.List as L


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
    go top@(high, mid, low) e
        | totalCalories e >= high = (totalCalories e, high, mid)
        | totalCalories e >= mid = (high, totalCalories e, mid)
        | totalCalories e >= low = (high, mid, totalCalories e)
        | otherwise = top


-- HELPERS

totalCalories :: Elf -> Int
totalCalories = sum . items


-- PARSE

newtype Elf = Elf
    { items :: [Int]
    }
    deriving (Show, Read, Eq, Ord)


elvesParser :: ReadP [Elf]
elvesParser =
    sepBy1 (Elf <$> sepBy1 parseInt newline) (newline *> newline)
        <* newline
        <* eof
