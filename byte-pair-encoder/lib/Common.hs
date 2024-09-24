module Common where

import           Data.List       (intercalate, elemIndex)
import           Data.List.Extra (splitOn)

tokenDictionary :: [String]
tokenDictionary = ["(0)", "(+1)", "(+2)", "(+3)", "(+4)", "(+5)", "(+6)", "(-1)", "(-2)", "(-3)", "(-4)", "(-5)", "M7", "m7", "7"]

notesOrder :: [String]
notesOrder = ["C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B"]

septs :: [String]
septs = ["M7", "m7", "7"]

ignoreWords :: [String]
ignoreWords = ["|", " "]

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
