module Day06 where

import Control.Monad
import Data.Char
import Data.MultiSet (MultiSet)
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.MultiSet as MS


-- (parseInput lineParser) OR (parseInputRaw fullInputParser)
main :: IO ()
main =
    getInputAndSolve
        (parseInputRaw parseBuffer)
        findStartOfPacketMarker
        findStartOfMessageMarker


-- SOLVE

findStartOfPacketMarker :: Buffer -> Int
findStartOfPacketMarker = findUniqueSequence 4


findStartOfMessageMarker :: Buffer -> Int
findStartOfMessageMarker = findUniqueSequence 14


-- HELPERS

findUniqueSequence :: Int -> Buffer -> Int
findUniqueSequence targetLength =
    either id (error . ("Unexpected result: " <>) . show)
        . foldM search (1, [], MS.empty)
        . fromBuffer
  where
    search :: (Int, String, MultiSet Char) -> Char -> Either Int (Int, String, MultiSet Char)
    search (ix, seen, seenSet) next
        | ix >= targetLength =
            let nextSeenSet = MS.insert next seenSet
                oldestSeen : restSeen = seen
             in if MS.uniqueSize nextSeenSet == targetLength
                    then Left ix
                    else Right (succ ix, restSeen <> [next], MS.delete oldestSeen nextSeenSet)
        | otherwise =
            Right (succ ix, seen <> [next], MS.insert next seenSet)


-- PARSE

newtype Buffer = Buffer
    { fromBuffer :: String
    }
    deriving (Show, Read, Eq, Ord)


parseBuffer :: ReadP Buffer
parseBuffer = Buffer <$> many (satisfy isAlpha) <* newline <* eof
