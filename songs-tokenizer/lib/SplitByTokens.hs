module SplitByTokens where

import Chords
import Safe (headMay)
import Data.Maybe
import           Data.List       (intercalate)
import           Data.List.Extra       (splitOn)


ignoreWords :: [String]
ignoreWords = ["|", " ", "NC"]

splitByDictionary :: String -> [String] -> [String]
splitByDictionary token (d:dict) = intercalate [d] restSplitted
    where
        restSplitted = map (`splitByDictionary` dict) $ splitOn d token
splitByDictionary _ [] = []

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
        go accum (w:wrdSong) (ch:chrds)
            | w `elem` ignoreWords = go (w:accum) wrdSong (ch:chrds)
            | normalizeChord (rawToChord $ removeBase w) == ch = go (w:accum) wrdSong chrds
            | otherwise = ([], wrdSong)
        go accum restSong [] = (reverse accum, restSong)
        go accum [] _        = (reverse accum, [])


splitByToken :: [String] -> [String] -> ([String], [String])
splitByToken wordedSong splitedToken = Data.Maybe.fromMaybe ([], wordedSong) mbSplit
    where
        potentialChords = map (`fromRelativeGivenRoot` splitedToken) notesOrder
        potentialSplits = map (splitByChords wordedSong) potentialChords
        mbSplit = headMay $ filter (not . null . fst) potentialSplits

chordsByTokens :: [String] -> [String] -> [String] -> [String]
chordsByTokens wordedSong tokenDictionary tokens = case mbGreedySplit of
                                   Nothing -> []
                                   Just (tokenized, rest) -> unwords tokenized : chordsByTokens rest tokenDictionary tokens
    where
        splitedTokens = map (`splitByDictionary` tokenDictionary) tokens
        splits = map (splitByToken wordedSong) splitedTokens
        mbGreedySplit = headMay $ filter (not . null . fst) splits
