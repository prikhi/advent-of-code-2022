{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Day02 where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Monad
import Data.Array (Array)
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.Function (on)
import Data.Functor
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Debug.Trace


main :: IO ()
main = getInputAndSolve (parseInput parseGame) idealScore targetResultScore


-- SOLVE

idealScore :: [Game] -> Int
idealScore = sum . map gameScore


targetResultScore :: [Game] -> Int
targetResultScore = sum . map calculateScore
  where
    calculateScore :: Game -> Int
    calculateScore g = gameScore g {me = findMoveForResult (opponent g) (translateMoveToResult $ me g)}

    translateMoveToResult :: Move -> GameResult
    translateMoveToResult = \case
        Rock -> Lose
        Paper -> Draw
        Scissors -> Win
    findMoveForResult :: Move -> GameResult -> Move
    findMoveForResult theirMove targetResult =
        case (targetResult, theirMove) of
            (Draw, _) -> theirMove
            (Win, Rock) -> Paper
            (Win, Paper) -> Scissors
            (Win, Scissors) -> Rock
            (Lose, Rock) -> Scissors
            (Lose, Paper) -> Rock
            (Lose, Scissors) -> Paper


-- HELPERS

data GameResult
    = Win
    | Lose
    | Draw
    deriving (Show, Read, Eq, Ord)


gameResult :: Game -> GameResult
gameResult Game {opponent, me} = case (me, opponent) of
    (Rock, Scissors) -> Win
    (Rock, Paper) -> Lose
    (Scissors, Paper) -> Win
    (Scissors, Rock) -> Lose
    (Paper, Rock) -> Win
    (Paper, Scissors) -> Lose
    _ -> Draw


gameScore :: Game -> Int
gameScore g@Game {me} =
    let resultScore = case gameResult g of
            Win -> 6
            Draw -> 3
            Lose -> 0
        moveScore = succ $ fromEnum me
     in resultScore + moveScore


-- PARSE

data Move
    = Rock
    | Paper
    | Scissors
    deriving (Show, Read, Eq, Ord, Enum, Bounded)


parseMove :: ReadP Move
parseMove =
    choice
        [ (char 'A' <|> char 'X') $> Rock
        , (char 'B' <|> char 'Y') $> Paper
        , (char 'C' <|> char 'Z') $> Scissors
        ]


data Game = Game
    { opponent :: Move
    , me :: Move
    }
    deriving (Show, Read, Eq, Ord)


parseGame :: ReadP Game
parseGame = do
    opponent <- parseMove
    void $ char ' '
    me <- parseMove
    return Game {..}
