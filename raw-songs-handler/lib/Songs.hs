{-# LANGUAGE DeriveAnyClass   #-}
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
contentToChords content = filterThreeRepeatingChords $ map (normalizeChord . rawToChord . removeBase) cordsRawNoNC
  where
    unbared = filter (/= '|') content
    cordsRaw = splitOn " " $ unwords $ words unbared
    cordsRawNoNC = filter (/= "NC") cordsRaw

    filterThreeRepeatingChords :: [Chord] -> [Chord]
    filterThreeRepeatingChords (c1:c2:c3:chords)
      | c1 == c2 && c2 == c3 = filterThreeRepeatingChords (c2:c3:chords)
      | otherwise = c1 : filterThreeRepeatingChords (c2:c3:chords)
    filterThreeRepeatingChords c = c

chordsToDiff :: [Chord] -> [String]
chordsToDiff chords = (concat . transpose) [gatheredSepts, relativeNoNotes]
  where
    relativeNoNotes = map (uncurry chordDiffShow) (makePairs chords)
    gatheredSepts = map septima chords

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


