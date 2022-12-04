-- | Helper functions for parsing problem inputs.
module ParseHelper where

import Data.Char (isDigit)
import Data.Functor
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Text.ParserCombinators.ReadP

import qualified GHC.Arr as A


-- | Run a parser on each line of the input file.
parseInput :: Show x => ReadP x -> String -> [x]
parseInput parser =
    parseInputRaw $ sepBy parser newline <* end
  where
    end = many $ choice [void newline, eof]


-- | Run a parser on the full input file.
parseInputRaw :: Show x => ReadP x -> String -> x
parseInputRaw parser str =
    let results = readP_to_S parser str
        successes = filter ((== "") . snd) results
        longestAttempts = sortOn (length . snd) results
     in case listToMaybe successes of
            Nothing ->
                error $ "Parsing failure, longest attempt: " <> show (head longestAttempts)
            Just success ->
                fst success


-- | Parse a newline or a carriage-return & newline.
newline :: ReadP Char
newline =
    choice
        [ char '\r' *> char '\n'
        , char '\n'
        ]


-- | Parse a positive or negative integer.
parseInt :: ReadP Int
parseInt = do
    sign <- option 1 (char '-' $> (-1))
    (sign *) . read <$> many1 (satisfy isDigit)


-- | Parse a comma-separated array of ints, with optional beginning & end
-- characters.
parseIntArray :: Maybe Char -> Maybe Char -> ReadP [Int]
parseIntArray maybeStart maybeEnd = do
    mapM_ char maybeStart
    ints <- sepBy parseInt (skipSpaces *> char ',' *> skipSpaces)
    mapM_ char maybeEnd
    return ints


-- | Parse a grid of digits with no column separators & rows separated by
-- newlines.
parseIntGrid :: ReadP (A.Array (Int, Int) Int)
parseIntGrid = do
    ls <- sepBy (many1 $ satisfy isDigit) newline <* newline
    let height = length ls
        width = minimum $ map length ls
    return $
        A.array
            ((0, 0), (height - 1, width - 1))
            [ ((w, h), c)
            | h <- [0 .. height - 1]
            , w <- [0 .. width - 1]
            , let c = read [ls !! h !! w]
            ]


-- | Parse a grid of characters.
parseCharGrid :: (Char -> Bool) -> ReadP (A.Array (Int, Int) Char)
parseCharGrid validChar = do
    ls <- sepBy (many1 $ satisfy validChar) newline
    let height = length ls
        width = minimum $ map length ls
    return $
        A.array
            ((0, 0), (width - 1, height - 1))
            [ ((w, h), c)
            | h <- [0 .. height - 1]
            , w <- [0 .. width - 1]
            , let c = ls !! h !! w
            ]
