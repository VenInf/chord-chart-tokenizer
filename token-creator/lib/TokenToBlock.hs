module TokenToBlock where

import           Chords        (applyDiffToNote, readDiff)
import           Data.List     (elemIndex)
import           SplitByTokens (splitToken, splitByDictionary)


tokenToBlock :: [String] -> String -> String
tokenToBlock tokensDictionary token = case mbRootNoteIndex of
                     Nothing -> token
                     Just (rootNote, index) -> do
                        let (prev, rootSept:next) = splitAt index splitToken
                        let block = unwords $ convertPrevToBlock rootNote prev <>
                                            [rootNote <> rootSept] <>
                                            convertNextToBlock rootNote next
                        block
    where
        splitToken = splitByDictionary token tokensDictionary -- TODO switch to splitToken, why fails?

        majorIndex = elemIndex "M7" splitToken
        minorIndex = elemIndex "m7" splitToken
        septIndex = elemIndex "7" splitToken

        halfdimIndex = elemIndex "m7b5" splitToken
        dimIndex = elemIndex "mb7b5" splitToken
        minorMajorIndex = elemIndex "mM7" splitToken

        mbRootNoteIndex = case majorIndex of
                          Just index -> Just ("C", index)
                          Nothing ->
                            case minorIndex of
                            Just index -> Just ("D", index)
                            Nothing ->
                              case septIndex of
                              Just index -> Just ("G", index)
                              Nothing ->
                                case halfdimIndex of
                                Just index -> Just ("C", index)
                                Nothing ->
                                  case dimIndex of
                                  Just index -> Just ("C", index)
                                  Nothing -> case minorMajorIndex of
                                    Just index -> Just ("C", index)
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
