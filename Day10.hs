{-# LANGUAGE ViewPatterns #-}

module Day10 where

import Data.Array (Array)
import Data.Foldable
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.Array as A


main :: IO ()
main =
    getInputAndSolve
        (concat <$> parseInput parseInstruction)
        (sumSignalStrengths [20, 60, 100, 140, 180, 220])
        drawCrt


-- SOLVE

sumSignalStrengths :: [Int] -> [Instruction] -> Int
sumSignalStrengths (map pred -> targetIxs) instructions =
    let cycleVals = calculateCycleValues instructions
     in sum $ map (\ix -> succ ix * cycleVals !! ix) targetIxs


drawCrt :: [Instruction] -> Grid
drawCrt instructions =
    let cycleVals = zip [0 ..] $ calculateCycleValues instructions
        initialCrt = A.listArray ((0, 0), (39, 5)) (replicate 240 False)
     in Grid $ foldl' drawPixels initialCrt cycleVals
  where
    drawPixels :: A.Array (Int, Int) Bool -> (Int, Int) -> A.Array (Int, Int) Bool
    drawPixels crt (pixelPos, spritePos) =
        let (pixelY, pixelX) = pixelPos `divMod` 40
         in if abs (pixelX - spritePos) <= 1
                then A.set [((pixelX, pixelY), True)] crt
                else crt


-- HELPERS

calculateCycleValues :: [Instruction] -> [Int]
calculateCycleValues = map fst . scanl go (1, Nothing)
  where
    go :: (Int, Maybe Int) -> Instruction -> (Int, Maybe Int)
    go (xVal, addCounter) instr =
        let (newXVal, newAddCounter) = case addCounter of
                Just toAdd ->
                    (xVal + toAdd, Nothing)
                Nothing ->
                    (xVal, Nothing)
         in case instr of
                Noop ->
                    (newXVal, newAddCounter)
                AddX toAdd ->
                    (newXVal, Just toAdd)


newtype Grid = Grid (Array (Int, Int) Bool)


instance Show Grid where
    show (Grid g) = "\n" <> A.showGridWith (\b -> if b then "#" else " ") g


-- PARSE

data Instruction
    = Noop
    | AddX Int
    deriving (Show, Read, Eq, Ord)


parseInstruction :: ReadP [Instruction]
parseInstruction =
    choice
        [ [Noop] <$ string "noop"
        , (: [Noop]) . AddX <$> (string "addx " *> parseInt)
        ]
