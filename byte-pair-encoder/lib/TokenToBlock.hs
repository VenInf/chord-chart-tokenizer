module TokenToBlock where

import           Data.List       (elemIndex)
import Common ( readDiff, applyDiffToNote, splitByDictionary, tokenDictionary )

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
        splitToken = splitByDictionary token tokenDictionary

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