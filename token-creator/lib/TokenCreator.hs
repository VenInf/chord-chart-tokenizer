{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE RecordWildCards #-}

module TokenCreator where

import           Chords
import           Data.Function   (on, (&))
import           Data.Hashable   (hash)
import           Data.List       (intercalate, isInfixOf, isPrefixOf,
                                  isSuffixOf, nub, sortBy, sortOn)
import           Data.List.Extra (splitOn)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust)
import SplitByTokens (splitToken)

type TokenID = Int

data TokenCreatorState = TokenCreatorState { texts        :: [String]
                                           , encodedTexts :: [[TokenID]]
                                           , decodeTable  :: [(TokenID, String)]
                                           } deriving (Show, Eq, Read)

toFst :: (b -> a) -> b -> (a, b)
toFst f a = (f a, a)

fmapToFst :: Functor f => (a -> b) -> f a -> f (b, a)
fmapToFst = fmap . toFst

encode :: String -> [(TokenID, String)] -> [TokenID]
encode txt (d:decodeTable) = intercalate [fst d] restEncoded
    where
        restEncoded = map (`encode` decodeTable) $ splitOn (snd d) txt
encode _ [] = []

decode :: [TokenID] -> [(TokenID, String)] -> String
decode encTxt decodeTable = unwords $ map (\t ->  fromJust $ lookup t decodeTable) encTxt


textToTokenizerState :: [String] -> TokenCreatorState
textToTokenizerState txts = TokenCreatorState {texts = txts, encodedTexts = encodedTxts, decodeTable = decodeTable}
    where
        dict = nub $ concatMap words txts
        decodeTable = sortOn (\(_, b) -> negate $ length b) $ fmapToFst hash dict
        encodedTxts = map (`encode` decodeTable) txts


tokenizerStateToTexts :: TokenCreatorState -> [String]
tokenizerStateToTexts (TokenCreatorState {..}) = map (`decode` decodeTable) encodedTexts


frequenciesOfElementsMap :: (Foldable t, Ord k, Num a) => t k -> Map.Map k a
frequenciesOfElementsMap = foldr (\elmnt counterMap -> Map.insertWith (+) elmnt 1 counterMap) Map.empty

tokenByValue :: String -> [(TokenID, String)] -> TokenID -- second value lookup
tokenByValue value decodeTable = fst $ head $ filter (\(_, v) -> v == value) decodeTable

topTokens :: TokenCreatorState -> [(TokenID, Int)]
topTokens TokenCreatorState {encodedTexts = encodedTexts} = rankings
    where frequencies = frequenciesOfElementsMap $ concat encodedTexts
          rankings = sortBy (flip compare `on` snd) (Map.toList frequencies)

humanReadebleRankings :: [(TokenID, Int)] -> TokenCreatorState -> [(String, Int)]
humanReadebleRankings rankings (TokenCreatorState {decodeTable = decodeTable}) = map (\(rID,rs) -> (fromJust $ lookup rID decodeTable, rs)) rankings

adjustFrequenciesByHeuristic :: (Ord k, Num a) => Map.Map k a -> (k -> Bool) -> a -> Map.Map k a
adjustFrequenciesByHeuristic frequencies isGood adjustDiff = Map.mapWithKey (\key freq -> if isGood key
                                                                               then freq + adjustDiff
                                                                               else freq)
                                                                               frequencies

mostFrequentTokenTriple :: TokenCreatorState -> Maybe (TokenID, TokenID, TokenID)
mostFrequentTokenTriple (TokenCreatorState { encodedTexts = tokens
                                           , decodeTable = decodeTable
                                           }) = if snd mostFrequent <= 0 then Nothing else Just $ fst mostFrequent
    where
        makeTriples (a:b:c:t) = (a, b, c) : makeTriples (b:c:t)
        makeTriples _         = []
        triples = concatMap makeTriples tokens

        frequenciesMap = frequenciesOfElementsMap triples -- still slow, but better
        totalFrequencies = length triples

        getAdjustDiff :: Float -> Int
        getAdjustDiff strength = floor $ strength * fromIntegral totalFrequencies

        adjustedFrequenciesMap = ($ frequenciesMap) (\f -> adjustFrequenciesByHeuristic f hasSeptsBounds (getAdjustDiff 1))
                                                  & (\f -> adjustFrequenciesByHeuristic f hasOnlyTerminalRepeats (getAdjustDiff 0.5))
                                                --   & (\f -> adjustFrequenciesByHeuristic f hasMajor (getAdjustDiff 0.05))



        maxBySnd p1@(_, v1) p2@(_, v2) = if v1 > v2 then p1 else p2
        mostFrequent = Map.foldrWithKey (\k1 n1 (k2, n2) -> maxBySnd (k1, n1) (k2, n2)) ((0, 0, 0), minBound :: Int) adjustedFrequenciesMap

        givePotentialToken :: (TokenID, TokenID, TokenID) -> String
        givePotentialToken (t1, t2, t3) = concat [ fromJust $ lookup t1 decodeTable
                                                 , fromJust $ lookup t2 decodeTable
                                                 , fromJust $ lookup t3 decodeTable
                                                 ]

        hasSeptsBounds :: (TokenID, TokenID, TokenID) -> Bool
        hasSeptsBounds (t1, t2, t3) = hasSeptPrefix && hasSeptSuffix
            where
                potentialToken = givePotentialToken (t1, t2, t3)

                hasSeptPrefix = any (`isPrefixOf` potentialToken) septs
                hasSeptSuffix = any (`isSuffixOf` potentialToken) septs

        hasRepeats :: (TokenID, TokenID, TokenID) -> Bool
        hasRepeats (t1, t2, t3) = "(0)" `isInfixOf` potentialToken
            where
                potentialToken = givePotentialToken (t1, t2, t3)

        hasOnlyTerminalRepeats :: (TokenID, TokenID, TokenID) -> Bool
        hasOnlyTerminalRepeats (t1, t2, t3) = case reverse splittedByDiffs of
                                          s1:"(0)":s2:_ -> s1 == s2 && length (filter (== "(0)") splittedByDiffs) == 1
                                          _ -> False
            where
                potentialToken = givePotentialToken (t1, t2, t3)
                splittedByDiffs = splitToken potentialToken

        hasMajor :: (TokenID, TokenID, TokenID) -> Bool
        hasMajor (t1, t2, t3) = "M7" `isInfixOf` potentialToken
            where
                potentialToken = givePotentialToken (t1, t2, t3)


        hasBlockLenN :: (TokenID, TokenID, TokenID) -> Int -> Bool
        hasBlockLenN (t1, t2, t3) n = length potentialToken == n * 2 - 1
            where
                potentialToken = givePotentialToken (t1, t2, t3)


addMergedToken :: (TokenID, TokenID, TokenID) -> TokenCreatorState -> TokenCreatorState
addMergedToken (t1, t2, t3) tState@(TokenCreatorState {decodeTable = decodeTable}) = tState {decodeTable = newDecodeTable}
    where
        newTokenID = hash (t1 + t2 + t3)
        tokenValue1 = fromJust $ lookup t1 decodeTable
        tokenValue2 = fromJust $ lookup t2 decodeTable
        tokenValue3 = fromJust $ lookup t3 decodeTable
        newDecodeTable = (newTokenID, tokenValue1 ++ tokenValue2 ++ tokenValue3) : decodeTable


mergeTokenTriple :: TokenCreatorState -> (TokenID, TokenID, TokenID) -> TokenCreatorState
mergeTokenTriple tState@(TokenCreatorState {encodedTexts = encodedTexts, decodeTable = decodeTable}) (t1, t2, t3) = newState
    where
        tokenValue1 = fromJust $ lookup t1 decodeTable
        tokenValue2 = fromJust $ lookup t2 decodeTable
        tokenValue3 = fromJust $ lookup t3 decodeTable
        newTokenValue = tokenValue1 ++ tokenValue2 ++ tokenValue3
        newTokenID = tokenByValue newTokenValue decodeTable

        replaceTriples (a:b:c:t)
          | a == t1 && b == t2 && c == t3 = newTokenID : replaceTriples t
          | otherwise = a : replaceTriples (b:c:t)
        replaceTriples ts = ts

        newState = tState {encodedTexts = map replaceTriples encodedTexts}

makeOneToken :: TokenCreatorState -> Maybe TokenCreatorState
makeOneToken tState = mbMergedTokenState
    where mbTokenTriple = mostFrequentTokenTriple tState
          mbAddedTokenState = (`addMergedToken` tState) <$> mbTokenTriple
          mbMergedTokenState = mergeTokenTriple <$> mbAddedTokenState <*> mbTokenTriple

makeNTokens :: TokenCreatorState -> Int -> TokenCreatorState
makeNTokens tState n
    | n > 0 = case makeOneToken tState of
              Nothing       -> tState
              Just newState -> makeNTokens newState (n-1)
    | otherwise = tState
