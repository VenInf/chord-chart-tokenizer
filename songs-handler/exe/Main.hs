{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import           Data.Aeson
import           Data.List
import           Data.List.Split
import           GHC.Generics
import           System.Directory
import           System.FilePath
import Common ( notesOrder )
import Chords
import SplitByTokens (chordsByTokens)

songsDir :: FilePath
songsDir = "./songs-handler/data"

tokensPath :: FilePath
tokensPath = "./songs-handler/tokens/tokens.txt"

data Song = Song {
      title      :: String
    , composedBy :: String
    , dbKeySig   :: String
    , timeSig    :: String
    , bars       :: String
    , content    :: String
    , chords     :: [Chord]
    , diffView   :: String
    , tokenView  :: [String]
    } deriving (Generic, Show, ToJSON, FromJSON)

newtype Songs
  = Songs {songs :: [Song]}
  deriving (Generic, Show, ToJSON, FromJSON)

showDiff :: Int -> String
showDiff n
  | n < -5    = showDiff (n + 12)
  | n > 6     = showDiff (n - 12)
  | n > 0     = "(+" ++ show n ++ ")"
  | otherwise = "(" ++ show n ++ ")"

contentToChords :: String -> [Chord]
contentToChords content = filterRepeatingChords $ map (normalizeChord . rawToChord) cordsRawNoNC
  where
    unbared = filter (/= '|') content
    cordsRaw = splitOn " " $ unwords $ words unbared
    cordsRawNoNC = filter (/= "NC") cordsRaw

    filterRepeatingChords :: [Chord] -> [Chord]
    filterRepeatingChords (c1:c2:chords)
      | c1 == c2 = filterRepeatingChords (c2:chords)
      | otherwise = c1: filterRepeatingChords (c2:chords)
    filterRepeatingChords c = c

chordsToDiff :: [Chord] -> [String]
chordsToDiff chords = (concat . transpose) [septs, relativeNoNotes]
  where
    chordDiff :: Chord -> Chord -> String
    chordDiff ch1@(Chord {note=n1}) ch2@(Chord {note=n2}) = showDiff $ pitch2 - pitch1
      where
        pitch1 = case elemIndex n1 notesOrder of
                 Nothing -> error (show ch1 ++ " encountered, failed to parse")
                 Just p -> p
        pitch2 = case elemIndex n2 notesOrder of
                 Nothing -> error (show ch2 ++ " encountered, failed to parse")
                 Just p -> p

    makePairs (a:b:t) = (a, b) : makePairs (b:t)
    makePairs _       = []

    relativeNoNotes = map (uncurry chordDiff) (makePairs chords)
    septs = map septima chords

contentToSong :: String -> [String] -> Song
contentToSong input tokens = go $ lines input
  where
    rmBeforeEqSign :: String -> String
    rmBeforeEqSign l = splitOn " = " l !! 1

    go :: [String] -> Song
    go (titleL:composedByL:dbKeySigL:timeSigL:barsL:contentL) = Song {..}
      where
        title = rmBeforeEqSign titleL
        composedBy = rmBeforeEqSign composedByL
        dbKeySig = rmBeforeEqSign dbKeySigL
        timeSig = rmBeforeEqSign timeSigL
        bars = rmBeforeEqSign barsL
        content = filter (/= '\n') $ unlines contentL
        chords =  contentToChords content
        diffView = concat $ chordsToDiff chords
        tokenView = chordsByTokens (words content) tokens
    go _ = error "unexpected number of lines"


main :: IO()
main = do
    songsFileNames <- listDirectory songsDir
    let songsPaths = [songsDir </> name | name <- songsFileNames]
    songsContents <- mapM readFile songsPaths

    rawTokens <- readFile tokensPath

    let tokens = lines rawTokens
        songs = map (`contentToSong` tokens) songsContents
        diffs = map diffView songs


    encodeFile "./songs-handler/out/allSongs.json" (Songs songs)
    putStrLn "Done"

    -- putStrLn "all possible symbols:"
    -- print $ sort $ nub $ words $ unlines diffs

    -- mapM_ putStrLn diffs

