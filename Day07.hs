{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Day07 where

import Control.Monad
import Data.Char
import Data.Foldable
import Data.Map (Map)
import Data.Maybe
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.List as L
import qualified Data.Map as M


-- (parseInput lineParser) OR (parseInputRaw fullInputParser)
main :: IO ()
main =
    getInputAndSolve
        (parseInputRaw (commandsToFileSystem <$> parseCommands))
        (sumDirectorySizeAtMost 100000)
        (sizeOfDirToDelete 70000000 30000000)


-- SOLVE

sumDirectorySizeAtMost :: Int -> FileSystem -> Int
sumDirectorySizeAtMost maxSizeToCount (FileSystem rootFs) =
    sum $ concatMap (snd . findMatchingDirSize) $ M.toList rootFs
  where
    findMatchingDirSize :: (String, FSNode) -> (Int, [Int])
    findMatchingDirSize = \case
        (_, FileNode {}) -> (0, [])
        (_, DirectoryNode (FileSystem dirFs)) ->
            let childDirSizes = map findMatchingDirSize $ M.toList dirFs
                childFileSizeSum =
                    sum
                        . map
                            ( ( \case
                                    FileNode s -> s
                                    DirectoryNode {} -> 0
                              )
                                . snd
                            )
                        $ M.toList dirFs
                totalDirSize = sum (map fst childDirSizes) + childFileSizeSum
                matchingChildDirSums = concatMap snd childDirSizes
             in if totalDirSize <= maxSizeToCount
                    then (totalDirSize, totalDirSize : matchingChildDirSums)
                    else (totalDirSize, matchingChildDirSums)


sizeOfDirToDelete :: Int -> Int -> FileSystem -> Int
sizeOfDirToDelete totalSpace spaceNeeded fs =
    fromMaybe (error $ "No directory big enough? Need to clear " <> show spaceToClear <> ".")
        . L.find (>= spaceToClear)
        . L.sort
        $ directorySizes fs
  where
    usedSpace :: Int
    usedSpace = totalUsedSpace fs
    spaceToClear :: Int
    spaceToClear = spaceNeeded - (totalSpace - usedSpace)


-- HELPERS

-- | A filesystem is a mapping from names to either files or directories.
newtype FileSystem = FileSystem
    { fromFileSystem :: Map String FSNode
    }
    deriving (Show, Read, Eq, Ord)


-- | A node in the file system tree can either be a directory containing
-- another sub-filesystem or a file with a size.
data FSNode
    = FileNode Int
    | DirectoryNode FileSystem
    deriving (Show, Read, Eq, Ord)


-- | Pretty print a file system.
showFileSystem :: FileSystem -> String
showFileSystem (FileSystem rootFs) =
    unlines $ go 0 "/" rootFs
  where
    go :: Int -> String -> Map String FSNode -> [String]
    go level dirName dirContents =
        let orderedContents = L.sortOn fst $ M.toList dirContents
            renderNode (nLabel, node) = case node of
                FileNode size ->
                    [replicate (succ level) ' ' <> "- " <> nLabel <> " (file, size=" <> show size <> ")"]
                DirectoryNode (FileSystem childFs) ->
                    go (succ level) nLabel childFs
            dirSize = totalUsedSpace $ FileSystem dirContents
         in (replicate level ' ' <> "- " <> dirName <> " (dir, size=" <> show dirSize <> ")")
                : concatMap renderNode orderedContents


-- | Determine the total space usage of a directory's file system.
totalUsedSpace :: FileSystem -> Int
totalUsedSpace (FileSystem rootFs) =
    foldl'
        ( \acc -> \case
            DirectoryNode childDir -> acc + totalUsedSpace childDir
            FileNode size -> acc + size
        )
        0
        rootFs


-- | List out all directory sizes for a directory & all it's child
-- directories.
directorySizes :: FileSystem -> [Int]
directorySizes (FileSystem fs) =
    concatMap collectSizes fs
  where
    collectSizes :: FSNode -> [Int]
    collectSizes = \case
        FileNode {} -> []
        DirectoryNode (FileSystem dirFs) ->
            let nodes = map snd $ M.toList dirFs
                fileSizes =
                    foldl'
                        ( \acc -> \case
                            FileNode s -> acc + s
                            DirectoryNode {} -> acc
                        )
                        0
                        nodes
                childDirSizes = map collectSizes nodes
                -- leverage the fact that the first element in each child
                -- dirs list is the total size of that child directory.
                childDirSizesSum = sum $ map (fromMaybe 0 . listToMaybe) childDirSizes
             in (fileSizes + childDirSizesSum) : concat childDirSizes


-- Generating FS from input data

-- | Generate a file system based on the input commands & their outputs.
commandsToFileSystem :: [Command] -> FileSystem
commandsToFileSystem = unZoom . foldl' runCommand initialZoomamble
  where
    runCommand :: Zoomable -> Command -> Zoomable
    runCommand z = \case
        ListDir fls ->
            createFileListings fls z
        ChangeDir "/" -> goToRoot z
        ChangeDir ".." ->
            fromMaybe (error $ "Could not enter parent: " <> show z) $
                parentDir z
        ChangeDir child -> enterDir child z


-- | This allows us to zoom into a directory, mess with it's filesystem,
-- & zoom out, apply any changes we made to the filesystem of it's parent.
data Zoomable = Zoomable
    { zParents :: [(String, FileSystem)]
    , zCurrentDir :: (String, FileSystem)
    }
    deriving (Show, Read, Eq, Ord)


-- | Start a root w/ an empty FS.
initialZoomamble :: Zoomable
initialZoomamble = Zoomable [] ("/", FileSystem M.empty)


-- | Zoom all the way out to the root file system, applying all pending
-- changes as we go.
unZoom :: Zoomable -> FileSystem
unZoom z = case parentDir z of
    Nothing ->
        snd $ zCurrentDir z
    Just parent -> unZoom parent


-- | Refocus to the base of the filesystem.
goToRoot :: Zoomable -> Zoomable
goToRoot = Zoomable [] . ("/",) . unZoom


-- | Go back up a directory in the tree, applying any changes to the
-- current directories file system to that parent's filesystem.
parentDir :: Zoomable -> Maybe Zoomable
parentDir Zoomable {..} = case zParents of
    [] -> Nothing
    (parentName, FileSystem parentFS) : otherAncestors ->
        let (curName, DirectoryNode -> curFS) = zCurrentDir
         in Just . Zoomable otherAncestors $
                ( parentName
                , FileSystem $ M.insert curName curFS parentFS
                )


-- | Enter an existing directory or create a new empty directory.
enterDir :: String -> Zoomable -> Zoomable
enterDir childDir Zoomable {..} =
    case M.lookup childDir $ fromFileSystem $ snd zCurrentDir of
        Just FileNode {} ->
            error $ "enterDir: tried entering into a file: " <> childDir
        Just (DirectoryNode childFs) ->
            Zoomable
                { zParents = zCurrentDir : zParents
                , zCurrentDir = (childDir, childFs)
                }
        Nothing ->
            Zoomable
                { zParents = zCurrentDir : zParents
                , zCurrentDir = (childDir, FileSystem M.empty)
                }


-- | Create files & folders in the current directory based on the output of
-- a 'ListDir' 'Command'.
createFileListings :: [FileListing] -> Zoomable -> Zoomable
createFileListings fls z =
    let (name, FileSystem oldFs) = zCurrentDir z
        newFs =
            foldr
                ( \(toNode -> (nodeName, node)) fs ->
                    M.insert nodeName node fs
                )
                oldFs
                fls
     in z
            { zCurrentDir = (name, FileSystem newFs)
            }
  where
    toNode :: FileListing -> (String, FSNode)
    toNode = \case
        File size name -> (name, FileNode size)
        Dir name -> (name, DirectoryNode $ FileSystem M.empty)


-- PARSE

parseCommands :: ReadP [Command]
parseCommands = sepBy parseCommand newline <* newline <* eof


data Command
    = ChangeDir String
    | ListDir [FileListing]
    deriving (Show, Read, Eq, Ord)


parseCommand :: ReadP Command
parseCommand = do
    void $ string "$ "
    choice
        [ ChangeDir <$> (string "cd " *> parseDirOrFileName)
        , ListDir <$> (string "ls" *> option ' ' newline *> sepBy parseFileNode newline)
        ]


data FileListing
    = File Int String
    | Dir String
    deriving (Show, Read, Eq, Ord)


parseFileNode :: ReadP FileListing
parseFileNode =
    choice
        [ File <$> parseInt <*> (char ' ' *> parseDirOrFileName)
        , Dir <$> (string "dir " *> parseDirOrFileName)
        ]


parseDirOrFileName :: ReadP String
parseDirOrFileName = many1 (satisfy $ \c -> isAlphaNum c || isPunctuation c)
