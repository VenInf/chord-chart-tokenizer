
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
septs = ["mb7b5", "m7b5", "mM7", "M7", "m7", "7"]

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

showChord :: Chord -> String
showChord (Chord {..}) = note <> septima

applyDiffToNote :: String -> Int -> String
applyDiffToNote note diff = notesOrder !! newIndex
    where
        noteIndex = case elemIndex note notesOrder of
                        Nothing -> error $ show note ++ " is not a valid note"
                        Just indx -> indx
        newIndex = (noteIndex + diff) `mod` length notesOrder


alterThirdsInChord :: Chord -> Chord
alterThirdsInChord (Chord {..}) = case septima of
    "7"     -> Chord note "7"
    "m7"    -> Chord note "m7b5"
    "m7b5"  -> Chord note "m7"
    "M7"    -> Chord note "mM7"
    "mM7"   -> Chord note "M7"
    "mb7b5" -> Chord note "mb7b5" -- unclear what the altered version should be
    _ -> error $ "Unknown septima " ++ septima


removeBase :: String -> String
removeBase rawChord = if '/' `elem` rawChord
                      then head $ splitOn "/" rawChord -- drop everyting after altered base
                      else rawChord


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
normalizeChord (Chord {..}) = Chord normNote (readSeptLessTrivial septima)
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


-- | Reads all possible septimas to
--   M7, m7 or 7
readSeptTrivial :: String -> String
readSeptTrivial spt
  | "6" `isPrefixOf` spt  = "M7"
  | "M" `isPrefixOf` spt  = "M7"
  | "m" `isPrefixOf` spt  = "m7"
  | "o7" `isPrefixOf` spt = "m7"
  | otherwise             = "7"


-- | Reads all possible septimas to
--   M7, m7, 7, m7b5 or mb7b5
--
-- Used notation vs other common variants
-- mb7b5  | o
-- m7b5   | ø
-- mM7    | mΔ
-- M7     | Δ
-- m7     | m7
-- 7      | 7
--
readSeptLessTrivial :: String -> String
readSeptLessTrivial spt
  | "mb7b5" `isInfixOf` spt                               = "mb7b5"
  | "dim" `isInfixOf` spt                                 = "mb7b5"
  | "o" `isPrefixOf` spt && not ("M7" `isInfixOf` spt)    = "mb7b5"
  | "7b5" `isInfixOf` spt && not ("M7b5" `isInfixOf` spt) = "m7b5"
  | "mM" `isInfixOf` spt                                  = "mM7"
  | "Maj" `isInfixOf` spt || "maj" `isInfixOf` spt        = "M7"
  | "M" `isInfixOf` spt                                   = "M7"
  | "m" `isInfixOf` spt                                   = "m7"
  | otherwise                                             = "7"
