module SplitByTokens where

import Common
import Chords
import Safe (headMay)
import Data.Maybe

fromRelativeGivenRoot :: String -> [String] -> [Chord]
fromRelativeGivenRoot _ [] = []
fromRelativeGivenRoot rootNote (r:rest)
    | r `elem` septs = Chord rootNote r : startingWithDiff rootNote rest
    | otherwise = startingWithDiff rootNote (r:rest)
    where
        startingWithDiff :: String -> [String] -> [Chord]
        startingWithDiff rNote (noteDiff:sept:relNotation) = Chord nextNote sept : startingWithDiff nextNote relNotation
            where nextNote = applyDiffToNote rNote (readDiff noteDiff)
        startingWithDiff rNote [noteDiff] = [Chord nextNote ""]
            where nextNote = applyDiffToNote rNote (readDiff noteDiff)
        startingWithDiff _ _ = []

splitByChords :: [String] -> [Chord] -> ([String], [String])
splitByChords wordedSong chords = go [] wordedSong chords
    where
        go :: [String] -> [String] -> [Chord] -> ([String], [String])
        go accum (w:wordedSong) (ch:chords)
            | w `elem` ignoreWords = go (w:accum) wordedSong (ch:chords)
            | normalizeChord (rawToChord w) == ch = go (w:accum) wordedSong chords
            | otherwise = ([], wordedSong)
        go accum restSong [] = (reverse accum, restSong)
        go _ [] _            = ([], wordedSong)


splitByToken :: [String] -> String -> ([String], [String])
splitByToken wordedSong token = Data.Maybe.fromMaybe ([], wordedSong) mbSplit
    where
        splitedToken = splitByDictionary token tokenDictionary
        potentialChords = map (`fromRelativeGivenRoot` splitedToken) notesOrder

        potentialSplits = map (splitByChords wordedSong) potentialChords
        mbSplit = headMay $ filter (not . null . fst) potentialSplits

chordsByTokens :: [String] -> [String] -> [String]
chordsByTokens wordedSong tokens = case mbGreedySplit of
                                   Nothing -> []
                                   Just (tokenized, rest) -> unwords tokenized : chordsByTokens rest tokens
    where
        splits = map (splitByToken wordedSong) tokens
        mbGreedySplit = headMay $ filter (not . null . fst) splits
