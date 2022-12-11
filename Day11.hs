{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Day11 where

import Control.Monad
import Data.Array (Array)
import Data.Foldable
import Data.Ord
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.Array as A
import qualified Data.List as L


-- (parseInput lineParser) OR (parseInputRaw fullInputParser)
main :: IO ()
main =
    getInputAndSolve
        (parseInputRaw fullInput)
        (calculateMonkeyBusiness (`div` 3) 20)
        (calculateMonkeyBusinessWithMod 10000)


-- SOLVE

calculateMonkeyBusinessWithMod :: Int -> [Monkey] -> Int
calculateMonkeyBusinessWithMod rounds monkeys =
    let divisors = product $ map mDivTest monkeys
     in calculateMonkeyBusiness (`mod` divisors) rounds monkeys


calculateMonkeyBusiness :: (Int -> Int) -> Int -> [Monkey] -> Int
calculateMonkeyBusiness worryMod rounds (A.fromList -> initialMonkeys) =
    product
        . take 2
        . L.sortOn Down
        . toList
        . A.amap mInspectionCount
        $ foldl' runRound initialMonkeys [0 .. rounds - 1]
  where
    -- Run a single round of worrying & throwing for all the monkeys.
    runRound :: Array Int Monkey -> a -> Array Int Monkey
    runRound monkeys _ =
        foldl' runMonkey monkeys [0 .. length monkeys - 1]

    -- Inspect & throw all the items for a monkey.
    runMonkey :: Array Int Monkey -> Int -> Array Int Monkey
    runMonkey monkeys turn =
        let monkey = monkeys A.! turn
            throws = map (inspect monkey) $ mItems monkey
            newMonkey =
                monkey
                    { mItems = []
                    , mInspectionCount = mInspectionCount monkey + length (mItems monkey)
                    }
         in foldl' throw (A.set [(turn, newMonkey)] monkeys) throws

    -- Determine the new worry level & where to throw the item.
    inspect :: Monkey -> Int -> (Int, Int)
    inspect Monkey {..} itemWorryLevel =
        let newWorryLevel = worryMod $ applyOp itemWorryLevel mOp
         in ( if newWorryLevel `mod` mDivTest == 0 then mTestTrue else mTestFalse
            , newWorryLevel
            )

    -- Apply the monkey's "new worry level" operation
    applyOp :: Int -> MonkeyOp -> Int
    applyOp initial = \case
        Add x -> initial + x
        Mult x -> initial * x
        Square -> initial * initial

    -- Throw an item to a monkey
    throw :: Array Int Monkey -> (Int, Int) -> Array Int Monkey
    throw monkeys (toMonkey, item) =
        let targetMonkey = monkeys A.! toMonkey
            newMonkey = targetMonkey {mItems = mItems targetMonkey <> [item]}
         in A.set [(toMonkey, newMonkey)] monkeys


-- HELPERS

-- PARSE

fullInput :: ReadP [Monkey]
fullInput = sepBy parseMonkey newline


data Monkey = Monkey
    { mItems :: [Int]
    , mOp :: MonkeyOp
    , mDivTest :: Int
    , mTestTrue :: Int
    , mTestFalse :: Int
    , mInspectionCount :: Int
    }
    deriving (Show, Read, Eq, Ord)


parseMonkey :: ReadP Monkey
parseMonkey = do
    void $ manyTill (satisfy (/= '\n')) newline
    mItems <- string "  Starting items: " *> sepBy parseInt (string ", ") <* newline
    mOp <- parseMonkeyOp <* newline
    mDivTest <- string "  Test: divisible by " *> parseInt <* newline
    mTestTrue <- string "    If true: throw to monkey " *> parseInt <* newline
    mTestFalse <- string "    If false: throw to monkey " *> parseInt <* newline
    let mInspectionCount = 0
    return Monkey {..}


data MonkeyOp
    = Mult Int
    | Add Int
    | Square
    deriving (Show, Read, Eq, Ord)


parseMonkeyOp :: ReadP MonkeyOp
parseMonkeyOp = do
    void $ string "  Operation: new = old "
    choice
        [ Square <$ string "* old"
        , Mult <$> (string "* " *> parseInt)
        , Add <$> (string "+ " *> parseInt)
        ]
