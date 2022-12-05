{-# LANGUAGE RecordWildCards #-}

module Day05 where

import Control.Monad
import Data.Array (Array)
import Data.Char
import Data.Foldable
import Data.Maybe
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.Array as A
import qualified Data.List as L


main :: IO ()
main = getInputAndSolve (parseInputRaw parseFullInput) (topOfStacks reverse) (topOfStacks id)


-- SOLVE

topOfStacks :: ([Crate] -> [Crate]) -> (Array Int CrateStack, [Instruction]) -> String
topOfStacks moveModifier (initial, instrs) =
    mapMaybe (fmap fromCrate . listToMaybe . fromStack)
        . toList
        $ L.foldl' move initial instrs
  where
    move :: Array Int CrateStack -> Instruction -> Array Int CrateStack
    move stacks Instruction {..} =
        let (toMove, remaining) = L.splitAt iCount . fromStack $ stacks A.! iSource
            newStack = CrateStack $ moveModifier toMove <> fromStack (stacks A.! iDest)
         in A.set [(iSource, CrateStack remaining), (iDest, newStack)] stacks


-- HELPERS

-- PARSE

parseFullInput :: ReadP (Array Int CrateStack, [Instruction])
parseFullInput = do
    crateRows <- sepBy parseCrateRow newline
    void $ many (choice [char ' ', satisfy isDigit]) <* newline
    void newline
    instructions <- sepBy parseInstruction newline
    void newline
    let stacks = map (CrateStack . catMaybes) $ L.transpose crateRows
    return
        ( A.listArray (0, length stacks - 1) stacks
        , instructions
        )


newtype CrateStack = CrateStack
    { fromStack :: [Crate]
    }
    deriving (Show, Read, Eq, Ord)


newtype Crate = Crate
    { fromCrate :: Char
    }
    deriving (Show, Read, Eq, Ord)


parseCrate :: ReadP Crate
parseCrate = do
    void $ char '['
    c <- satisfy isAsciiUpper
    void $ char ']'
    return $ Crate c


parseMaybeCrate :: ReadP (Maybe Crate)
parseMaybeCrate =
    choice
        [ Just <$> parseCrate
        , Nothing <$ count 3 (char ' ')
        ]


parseCrateRow :: ReadP [Maybe Crate]
parseCrateRow = sepBy parseMaybeCrate (char ' ')


data Instruction = Instruction
    { iCount :: Int
    , iSource :: Int
    , iDest :: Int
    }
    deriving (Show, Read, Eq, Ord)


parseInstruction :: ReadP Instruction
parseInstruction = do
    void $ string "move "
    iCount <- parseInt
    void $ string " from "
    iSource <- pred <$> parseInt
    void $ string " to "
    iDest <- pred <$> parseInt
    return Instruction {..}
