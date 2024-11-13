
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Chords where

import           Data.Aeson
import           Data.List
import           Data.List.Split
import           GHC.Generics

data Chord = Chord { note    :: String
                   , septima :: String
                   } deriving (Generic, Show, Eq, ToJSON, FromJSON)

notesOrder :: [String]
notesOrder = ["C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B"]

septs :: [String]
septs = ["mb7b5", "m7b5", "mM7", "M7", "m7", "7"]

diffs :: [String]
diffs = ["(+6)", "(+5)", "(+4)", "(+3)", "(+2)", "(+1)", "(0)", "(-1)", "(-2)", "(-3)", "(-4)", "(-5)"]

showDiff :: Int -> String
showDiff d
  | nd > 0     = "(+" ++ show nd ++ ")"
  | otherwise = "(" ++ show nd ++ ")"
  where nd = normDiff d

readDiff :: String -> Int
readDiff diff = go diffNoParenthesis
    where
        diffNoParenthesis = init $ tail diff
        go ('+':numb) = read numb
        go numb       = read numb

readDiffHardcoded :: String -> Int
readDiffHardcoded "(+6)" = 6
readDiffHardcoded "(+5)" = 5
readDiffHardcoded "(+4)" = 4
readDiffHardcoded "(+3)" = 3
readDiffHardcoded "(+2)" = 2
readDiffHardcoded "(+1)" = 1
readDiffHardcoded "(0)"  = 0
readDiffHardcoded "(-1)" = -1
readDiffHardcoded "(-2)" = -2
readDiffHardcoded "(-3)" = -3
readDiffHardcoded "(-4)" = -4
readDiffHardcoded "(-5)" = -5
readDiffHardcoded d = error $ "Unable to parse diff " ++ d


showChord :: Chord -> String
showChord (Chord {..}) = note <> septima

applyDiffToNote :: String -> Int -> String
applyDiffToNote note diff = notesOrder !! newIndex
    where
        noteIndex = case elemIndex note notesOrder of
                        Nothing   -> error $ show note ++ " is not a valid note"
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
    _       -> error $ "Unknown septima " ++ septima


removeBase :: String -> String
removeBase rawChord = if '/' `elem` rawChord
                      then head $ splitOn "/" rawChord -- drop everyting after altered base
                      else rawChord

rawToChord :: String -> Chord
rawToChord chordRaw =
  case chordRaw of
    []              -> Chord [] []
    [note, 'b']     -> Chord [note, 'b'] "M" -- we will note major as M
    [note, '#']     -> Chord [note, '#'] "M"
    [note]          -> Chord [note] "M"
    (note:'b':sept) -> Chord [note, 'b'] sept
    (note:'#':sept) -> Chord [note, '#'] sept
    (note:sept)     -> Chord [note] sept

isChord :: Chord -> Bool
isChord (Chord note septima) = note `elem` notesOrder && septima `elem` septs

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

              n    -> n

chordDiff :: Chord -> Chord -> Int
chordDiff ch1@(Chord {note=n1}) ch2@(Chord {note=n2}) = normDiff $ pitch2 - pitch1
  where
    pitch1 = case elemIndex n1 notesOrder of
              Nothing -> error (show ch1 ++ " encountered, failed to parse")
              Just p  -> p
    pitch2 = case elemIndex n2 notesOrder of
              Nothing -> error (show ch2 ++ " encountered, failed to parse")
              Just p  -> p

normDiff :: Int -> Int
normDiff d
  | d < -5    = normDiff (d + 12)
  | d > 6     = normDiff (d - 12)
  | otherwise = d


chordDiffShow:: Chord -> Chord -> String
chordDiffShow ch1 ch2 = showDiff $ chordDiff ch1 ch2

similarity :: Chord -> Chord -> Float
similarity ch1 ch2 = 1 - fromIntegral (septDiff + abs (chordDiff ch1 ch2)) / fromIntegral (length notesOrder + 1)
  where
    septDiff = if septima ch1 == septima ch2 then 0 else 1



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
