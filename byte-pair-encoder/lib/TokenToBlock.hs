module TokenToBlock where

import           Data.Function   (on)
import           Data.Hashable   (hash)
import           Data.List       (intercalate, nub, sortBy, sortOn, elemIndex)
import           Data.List.Extra (splitOn)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust)

dictionary :: [String]
dictionary = ["(0)", "(+1)", "(+2)", "(+3)", "(+4)", "(+5)", "(+6)", "(-1)", "(-2)", "(-3)", "(-4)", "(-5)", "M7", "m7", "7"]

notesOrder :: [String]
notesOrder = ["C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B"]

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


splitByDictionary :: String -> [String] -> [String]
splitByDictionary token (d:dict) = intercalate [d] restSplitted
    where
        restSplitted = map (`splitByDictionary` dict) $ splitOn d token
splitByDictionary _ [] = []

{--
M7 -> CM7
m7 -> Dm
7 -> G7
--}

tokenToBlock :: String -> String
tokenToBlock token = case mbRootNoteIndex of
                     Nothing -> token
                     Just (rootNote, index) -> do
                        let (prev, rootSept:next) = splitAt index splitToken
                        let block = unwords $ convertPrevToBlock rootNote prev <>
                                            [rootNote <> rootSept] <>
                                            convertNextToBlock rootNote next
                        block
    where
        splitToken = splitByDictionary token dictionary

        majorIndex = elemIndex "M7" splitToken
        minorIndex = elemIndex "m7" splitToken
        septIndex = elemIndex "7" splitToken

        mbRootNoteIndex = case majorIndex of
                          Just index -> Just ("C", index)
                          Nothing ->
                            case minorIndex of
                            Just index -> Just ("D", index)
                            Nothing ->
                                case septIndex of
                                Just index -> Just ("G", index)
                                Nothing -> Nothing

convertNextToBlock :: String -> [String] -> [String]
convertNextToBlock rootNote (noteDiff:sept:relNotation) = (nextNote <> sept) : convertNextToBlock nextNote relNotation
    where nextNote = applyDiffToNote rootNote (readDiff noteDiff)
convertNextToBlock rootNote [noteDiff] = [nextNote <> "?"]
    where nextNote = applyDiffToNote rootNote (readDiff noteDiff)
convertNextToBlock _ _ = []


convertPrevToBlock :: String -> [String] -> [String]
convertPrevToBlock rootNote relNotation = reverse $ go rootNote (reverse relNotation)
    where
        go rNote (noteDiff:sept:rest) = (prevNote <> sept) : go prevNote rest
            where prevNote = applyDiffToNote rNote (negate $ readDiff noteDiff)
        go rNote [noteDiff] = [prevNote <> "?"]
            where prevNote = applyDiffToNote rNote (negate $ readDiff noteDiff)
        go _ _ = []