module SplitByTokens where

import           Chords
import           Data.List       (intercalate, nub, sortOn)
import           Data.List.Extra (splitOn)
import           Data.List.Split (split, whenElt)
import           Data.Maybe
import           Safe            (headMay)


ignoreWords :: [String]
ignoreWords = ["|", " ", "NC"]

splitByDictionary :: String -> [String] -> [String]
splitByDictionary token (d:dict) = intercalate [d] restSplitted
    where
        restSplitted = map (`splitByDictionary` dict) $ splitOn d token
splitByDictionary _ [] = []

splitToken :: String -> [String]
splitToken token = filter (/= "") $ go splitPars ""
    where
        splitPars = split (whenElt (`elem` "()")) token

        go :: [String] -> String -> [String]
        go ("(" : rest) accum = accum : go rest "("
        go (")" : rest) accum = (accum ++ ")") : go rest []
        go (s : rest) accum   = go rest (accum ++ s)
        go [] accum           = [accum]

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
splitByChords = go []
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

makeTokensDictionary :: [String] -> [String]
makeTokensDictionary diffs = sortOn ((* (-1)) . length) $ nub $ concatMap words diffs

chordsByTokens :: [String] -> [String] -> [String] -> [String]
chordsByTokens wordedSong tokensDictionary tokens = case mbGreedySplit of
                                   Nothing -> []
                                   Just (tokenized, rest) -> unwords tokenized : chordsByTokens rest tokensDictionary tokens
    where
        splitedTokens = map (`splitByDictionary` tokensDictionary) tokens
        splits = map (splitByToken wordedSong) splitedTokens
        mbGreedySplit = headMay $ filter (not . null . fst) splits
