{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Songs where

import Chords
import           Data.Aeson
import           Data.List
import           Data.List.Split
import           GHC.Generics

data Song = Song {
      title      :: String
    , composedBy :: String
    , dbKeySig   :: String
    , timeSig    :: String
    , bars       :: String
    , content    :: String
    , chords     :: [Chord]
    , diffView   :: String
    , tokenView  :: Maybe [String]
    , blockView  :: Maybe [String]
    } deriving (Generic, Show, ToJSON, FromJSON)

newtype Songs
  = Songs {songs :: [Song]}
  deriving (Generic, Show, ToJSON, FromJSON)

contentToChords :: String -> [Chord]
contentToChords content = map (normalizeChord . rawToChord) cordsRawNoNC
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
    relativeNoNotes = map (uncurry chordDiff) (makePairs chords)
    septs = map septima chords

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
        tokenView = Nothing
        blockView = Nothing
    go _ = error "unexpected number of lines"


