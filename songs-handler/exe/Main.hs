{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import           Data.Aeson
import           Data.Char
import           Data.List
import           Data.List.Split
import           GHC.Generics
import           System.Directory
import           System.FilePath

songsDir :: FilePath
songsDir = "./data"

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
} deriving (Generic, Show, ToJSON, FromJSON)

showDiff :: Int -> String
showDiff n = if n > 0
             then "(+" ++ show n ++ ")"
             else "(" ++ show n ++ ")"

contentToChords :: String -> [Chord]
contentToChords content = map (normalizeChord . rawToChord) cordsRawNoNC
  where
    unbared = filter (/= '|') content
    cordsRaw = splitOn " " $ unwords $ words unbared
    cordsRawNoNC = filter (/= "NC") cordsRaw

    rawToChord :: String -> Chord
    rawToChord chordRaw =
      case chordRaw of
        [] -> Chord [] []
        [note, 'b'] -> Chord [note, 'b'] "_"
        [note, '#'] -> Chord [note, '#'] "_"
        [note] -> Chord [note] "_"
        (note:'b':sept) -> Chord [note, 'b'] sept
        (note:'#':sept) -> Chord [note, '#'] sept
        (note:sept) -> Chord [note] sept

normalizeChord :: Chord -> Chord
normalizeChord (Chord {..}) = Chord normNote noAltBaseSept
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
               then head $ splitOn "/" sept -- give everyting before altered base
               else sept


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


makeContentRelative :: String -> String
makeContentRelative content = unwords $ joinRaw relativeRaw
    where
        unbared = filter (/= '|') content
        cords = splitOn " " $ unwords $ words unbared
        cordPairs = makePairs cords
        relativeRaw = map twoCordsToRelative cordPairs

        makePairs (a:b:t) = (a, b) : makePairs (b:t)
        makePairs _       = []

        joinRaw :: [(String, String, String)] -> [String]
        joinRaw [(sept1, df1, common1), (common2, df2, sept2)] =
            if common1 == common2
            then [sept1, df1, common1, df2, sept2]
            else error $ "common 1 (" ++ show common1 ++ ") and common 2 (" ++ show common2 ++ "aren't equal"
        joinRaw ((sept1, df1, common1):(common2, df2, sept2):rest) =
            if common1 == common2
            then [sept1, df1] ++ joinRaw ((common2, df2, sept2):rest)
            else error $ "common 1 (" ++ show common1 ++ ") and common 2 (" ++ show common2 ++ "aren't equal"
        joinRaw _ = []

twoCordsToRelative :: (String, String) -> (String, String, String)
twoCordsToRelative (note1:sept1, note2:sept2) = (sept1, showDiff $ ord note2 - ord note1, sept2)
twoCordsToRelative _ = error "wrong chords format"

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
        -- diffView = []
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
    -- mapM_ putStrLn diffs
    putStrLn "all possible symbols:"
    print $ sort $ nub $ words $ unlines diffs
    putStrLn "Done"
