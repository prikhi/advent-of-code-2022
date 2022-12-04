{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Day04 where

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
main = getInputAndSolve (parseInput parseElfPair) countFullOverlaps countAnyOverlaps


-- SOLVE

countFullOverlaps :: [(SectionRange, SectionRange)] -> Int
countFullOverlaps = length . filter isFullOverlap
  where
    isFullOverlap :: (SectionRange, SectionRange) -> Bool
    isFullOverlap (SectionRange e1Start e1End, SectionRange e2Start e2End) =
        (e1Start >= e2Start && e1End <= e2End)
            || (e2Start >= e1Start && e2End <= e1End)


countAnyOverlaps :: [(SectionRange, SectionRange)] -> Int
countAnyOverlaps = length . filter hasOverlap
  where
    hasOverlap :: (SectionRange, SectionRange) -> Bool
    hasOverlap (SectionRange e1Start e1End, SectionRange e2Start e2End) =
        or
            [ e1Start <= e2Start && e1End >= e2Start
            , e2Start <= e1Start && e2End >= e1Start
            , e1Start >= e2Start && e1End <= e2End
            , e2Start >= e1Start && e2End <= e1End
            ]


-- HELPERS

-- PARSE

data SectionRange = SectionRange
    { srStart :: Int
    , srEnd :: Int
    }
    deriving (Show, Read, Eq, Ord)


parseSectionRange :: ReadP SectionRange
parseSectionRange = do
    srStart <- parseInt
    void $ char '-'
    srEnd <- parseInt
    return SectionRange {..}


parseElfPair :: ReadP (SectionRange, SectionRange)
parseElfPair = do
    ep1 <- parseSectionRange
    void $ char ','
    ep2 <- parseSectionRange
    return (ep1, ep2)
