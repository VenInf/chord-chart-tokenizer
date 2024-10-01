
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Chords where

import           Data.Aeson
import           Data.List
import           Data.List.Split
import           GHC.Generics

data Chord = Chord {
      note :: String
    , septima :: String
} deriving (Generic, Show, Eq, ToJSON, FromJSON)

notesOrder :: [String]
notesOrder = ["C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B"]

septs :: [String]
septs = ["M7", "m7", "7"]

showDiff :: Int -> String
showDiff n
  | n < -5    = showDiff (n + 12)
  | n > 6     = showDiff (n - 12)
  | n > 0     = "(+" ++ show n ++ ")"
  | otherwise = "(" ++ show n ++ ")"

readDiff :: String -> Int
readDiff diff = go diffNoParenthesis
    where
        diffNoParenthesis = init $ tail diff
        go ('+':numb) = read numb
        go numb = read numb

applyDiffToNote :: String -> Int -> String
applyDiffToNote note diff = notesOrder !! newIndex
    where
        noteIndex = case elemIndex note notesOrder of
                        Nothing -> error $ show note ++ " is not a valid note"
                        Just indx -> indx
        newIndex = (noteIndex + diff) `mod` length notesOrder

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

    noAltBaseSept = if '/' `elem` septima
               then head $ splitOn "/" septima -- drop everyting after altered base
               else septima

    trivializeSept spt
      | "6" `isPrefixOf` spt  = "M7"
      | "M" `isPrefixOf` spt  = "M7"
      | "m" `isPrefixOf` spt  = "m7"
      | "o7" `isPrefixOf` spt = "m7"
      | otherwise             = "7"

