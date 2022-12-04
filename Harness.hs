-- | Helpers for parsing inputs & running solutions w/ timing information.
module Harness where

import Control.Exception (evaluate)
import Data.Time (diffUTCTime, getCurrentTime)


-- | Read the entire input from stdin.
getRawInput :: IO String
getRawInput =
    getContents >>= evaluate


-- | Parse the input, run the part 1 & 2 solvers, print the solutions with
-- an optional label & some timing information.
solve :: (Show b, Show c) => String -> (String -> a) -> (a -> b) -> (a -> c) -> String -> IO ()
solve rawLabel parser p1Solver p2Solver input = do
    let label = if null rawLabel then "" else " (" <> rawLabel <> ")"

    t1 <- getCurrentTime
    parseResult <- evaluate $ parser input
    t2 <- getCurrentTime
    putStrLn $ "Part 1" <> label <> ": " <> show (p1Solver parseResult)
    t3 <- getCurrentTime
    putStrLn $ "Part 2" <> label <> ": " <> show (p2Solver parseResult)
    t4 <- getCurrentTime
    mapM_
        putStrLn
        [ "Timings:"
        , "\tParsing:\t" <> show (diffUTCTime t2 t1)
        , "\tPart 1: \t" <> show (diffUTCTime t3 t2)
        , "\tPart 2: \t" <> show (diffUTCTime t4 t3)
        , "\tTotal:  \t" <> show (diffUTCTime t4 t1)
        ]


-- | Read & parse the input, solve both parts of the problem, & print out
-- the solutions.
getInputAndSolve :: (Show b, Show c) => (String -> a) -> (a -> b) -> (a -> c) -> IO ()
getInputAndSolve parser p1Solver p2Solver =
    getRawInput >>= solve "" parser p1Solver p2Solver
