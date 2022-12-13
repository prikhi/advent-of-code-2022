{-# LANGUAGE LambdaCase #-}

module Day13 where

import Control.Monad.Except (Except, runExcept, throwError)
import Data.Either
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.List as L


main :: IO ()
main =
    getInputAndSolve
        (parseInputRaw parseFullInput)
        correctOrderIxSum
        calculateDecoderKey


-- SOLVE

correctOrderIxSum :: [(Packet, Packet)] -> Int
correctOrderIxSum =
    sum
        . map fst
        . filter snd
        . zip [1 ..]
        . map (fromLeft (error "invalid - got equal ordering") . runExcept . isOrdered)


calculateDecoderKey :: [(Packet, Packet)] -> Int
calculateDecoderKey =
    let extras =
            [ PList [PList [PInt 2]]
            , PList [PList [PInt 6]]
            ]
     in product
            . map fst
            . filter ((`elem` extras) . snd)
            . zip [1 ..]
            . L.sortBy compareOrdering
            . (extras <>)
            . uncurry (<>)
            . unzip


-- HELPERS

isOrdered :: (Packet, Packet) -> Except Bool ()
isOrdered = \case
    (p1@(PInt _), p2@(PList _)) ->
        isOrdered (PList [p1], p2)
    (p1@(PList _), p2@(PInt _)) ->
        isOrdered (p1, PList [p2])
    (PInt p1, PInt p2) -> case compare p1 p2 of
        LT -> throwError True
        GT -> throwError False
        EQ -> return ()
    (PList [], PList []) ->
        return ()
    (PList [], PList _) ->
        throwError True
    (PList _, PList []) ->
        throwError False
    (PList [p1], PList [p2]) ->
        isOrdered (p1, p2)
    (PList (p1 : p1rest), PList (p2 : p2rest)) ->
        isOrdered (p1, p2) >> isOrdered (PList p1rest, PList p2rest)


compareOrdering :: Packet -> Packet -> Ordering
compareOrdering p1 p2 = case runExcept $ isOrdered (p1, p2) of
    Left True -> LT
    Left False -> GT
    Right () -> EQ


-- PARSE

parseFullInput :: ReadP [(Packet, Packet)]
parseFullInput = sepBy parsePacketPair (newline <* newline) <* newline <* eof


parsePacketPair :: ReadP (Packet, Packet)
parsePacketPair =
    (,) <$> parsePacket <*> (newline *> parsePacket)


data Packet
    = PInt Int
    | PList [Packet]
    deriving (Show, Read, Eq, Ord)


parsePacket :: ReadP Packet
parsePacket =
    choice
        [ PList <$> between (char '[') (char ']') (sepBy parsePacket $ char ',')
        , PInt <$> parseInt
        ]
