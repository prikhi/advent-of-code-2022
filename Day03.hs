{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Day03 where

import Data.Char
import Data.Function (on)
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.List as L


main :: IO ()
main =
    getInputAndSolve
        (parseInput parseSack)
        commonItemsPriority
        elfGroupPriority


-- SOLVE

commonItemsPriority :: [Sack] -> Int
commonItemsPriority = sum . map (toPriority . findCommon)
  where
    findCommon :: Sack -> Char
    findCommon (Sack items) =
        let (firstCompartment, secondCompartment) =
                L.splitAt (length items `div` 2) items
         in head $ firstCompartment `L.intersect` secondCompartment


elfGroupPriority :: [Sack] -> Int
elfGroupPriority =
    sum
        . map (toPriority . findCommon . map snd)
        . L.groupBy ((==) @Integer `on` (`div` 3) . fst)
        . zip [0 ..]
  where
    findCommon :: [Sack] -> Char
    findCommon = \case
        [Sack a, Sack b, Sack c] ->
            head $ a `L.intersect` b `L.intersect` c
        err -> error $ "Group w/ length not 3: " <> show err


-- HELPERS

toPriority :: Char -> Int
toPriority c
    | isAsciiLower c = fromEnum c - 96
    | otherwise = fromEnum c - 38


-- PARSE

newtype Sack = Sack [Char] deriving (Show, Read, Eq, Ord)


parseSack :: ReadP Sack
parseSack = Sack <$> many (satisfy isAlpha)
