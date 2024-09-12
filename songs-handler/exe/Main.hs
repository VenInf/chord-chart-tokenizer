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

songsDir :: FilePath
songsDir = "./songs-handler/data"

notesOrder :: [String]
notesOrder = ["C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B"]


data Song = Song {
      title      :: String
    , composedBy :: String
    , dbKeySig   :: String
    , timeSig    :: String
    , bars       :: String
    , content    :: String
    , chords     :: [Chord]
    , diffView   :: String
    } deriving (Generic, Show, ToJSON, FromJSON)

newtype Songs
  = Songs {songs :: [Song]}
  deriving (Generic, Show, ToJSON, FromJSON)

data Chord = Chord {
      note :: String
    , sept :: String
} deriving (Generic, Show, Eq, ToJSON, FromJSON)

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

    rawToChord :: String -> Chord
    rawToChord chordRaw =
      case chordRaw of
        [] -> Chord [] []
        [note, 'b'] -> Chord [note, 'b'] "M" -- we will note major as M
        [note, '#'] -> Chord [note, '#'] "M"
        [note] -> Chord [note] "M"
        (note:'b':sept) -> Chord [note, 'b'] sept
        (note:'#':sept) -> Chord [note, '#'] sept
        (note:sept) -> Chord [note] sept

    filterRepeatingChords :: [Chord] -> [Chord]
    filterRepeatingChords (c1:c2:chords)
      | c1 == c2 = filterRepeatingChords (c2:chords)
      | otherwise = c1: filterRepeatingChords (c2:chords)
    filterRepeatingChords c = c

normalizeChord :: Chord -> Chord
normalizeChord (Chord {..}) = Chord normNote (trivializeSept noAltBaseSept)
  where
    normNote = case note of
              "Fb" -> "E"
              "Cb" -> "B"

              "C#" -> "Db"
              "D#" -> "Eb"
              "E#" -> "F"
              "F#" -> "Gb"
              "G#" -> "Ab"
              "A#" -> "Bb"
              "B#" -> "C"

              n -> n

    noAltBaseSept = if '/' `elem` sept
               then head $ splitOn "/" sept -- drop everyting after altered base
               else sept

    trivializeSept spt
      | "6" `isPrefixOf` spt  = "M7"
      | "M" `isPrefixOf` spt  = "M7"
      | "m" `isPrefixOf` spt  = "m7"
      | "o7" `isPrefixOf` spt = "m7"
      | otherwise             = "7"


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
    septs = map sept chords

contentToSong :: String -> Song
contentToSong input = go $ lines input
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
        diffView = unwords $ chordsToDiff chords
    go _ = error "unexpected number of lines"


main :: IO()
main = do
    songsFileNames <- listDirectory songsDir
    let songsPaths = [songsDir </> name | name <- songsFileNames]
    songsContents <- mapM readFile songsPaths

    let songs = map contentToSong songsContents
    let diffs = map diffView songs

    -- encodeFile "./out/allSongs.json" (Songs songs)

    mapM_ putStrLn diffs

    -- putStrLn "all possible symbols:"
    -- print $ sort $ nub $ words $ unlines diffs

    -- putStrLn "Done"
