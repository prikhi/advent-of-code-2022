{-# LANGUAGE LambdaCase #-}

module Day09 where

import Data.Bifunctor
import Data.Foldable
import Data.Set (Set)
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.Set as S


main :: IO ()
main =
    getInputAndSolve
        (concat . parseInput parseMovement)
        (countUniqueLongTailSpots 2)
        (countUniqueLongTailSpots 10)


-- SOLVE

countUniqueLongTailSpots :: Int -> [Movement] -> Int
countUniqueLongTailSpots knotCount =
    length . snd . foldl' go (replicate knotCount (0, 0), S.empty)
  where
    go :: ([(Int, Int)], Set (Int, Int)) -> Movement -> ([(Int, Int)], Set (Int, Int))
    go (knotPos, seenTails) movement =
        let (headPos : tailPos) = knotPos
            newPos@(lastTailPos : _) =
                foldl'
                    ( \acc@(lastMoved : _) toMove ->
                        moveTail lastMoved toMove : acc
                    )
                    [moveHead movement headPos]
                    tailPos
         in (reverse newPos, S.insert lastTailPos seenTails)


-- HELPERS

-- | Move the leading knot according to the input direction.
moveHead :: Movement -> (Int, Int) -> (Int, Int)
moveHead = \case
    U -> second succ
    R -> first succ
    D -> second pred
    L -> first pred


-- | Move a tail knot by following it's leading knot.
moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail (hX, hY) tPos@(tX, tY) =
    if abs (tX - hX) <= 1 && abs (tY - hY) <= 1
        then tPos
        else
            let mkMod h t = case compare h t of
                    EQ -> id
                    LT -> pred
                    GT -> succ
             in bimap (mkMod hX tX) (mkMod hY tY) tPos


-- PARSE

data Movement
    = U
    | R
    | D
    | L
    deriving (Show, Read, Eq, Ord)


parseMovement :: ReadP [Movement]
parseMovement = do
    move <-
        choice
            [ U <$ string "U "
            , R <$ string "R "
            , D <$ string "D "
            , L <$ string "L "
            ]
    c <- parseInt
    return $ replicate c move
