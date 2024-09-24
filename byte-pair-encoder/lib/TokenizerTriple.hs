{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module TokenizerTriple where

import Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust)
import Common

import Tokenizer
    ( TokenizerState(TokenizerState, encodedTexts, decodeTable),
      TokenID,
      frequenciesOfElementsMap,
      tokenByValue )

adjustFrequenciesByHeuristic :: (Ord k, Num a) => Map.Map k a -> (k -> Bool) -> Map.Map k a
adjustFrequenciesByHeuristic frequencies isGood = Map.mapWithKey (\key freq -> if isGood key then freq + adjustDiff else freq) frequencies
    where
        adjustDiff = fromIntegral $ length frequencies

mostFrequentTokenTriple :: TokenizerState -> (TokenID, TokenID, TokenID)
mostFrequentTokenTriple (TokenizerState {encodedTexts = tokens, decodeTable = decodeTable}) = fst mostFrequent
    where
        makeTriples (a:b:c:t) = (a, b, c) : makeTriples (b:c:t)
        makeTriples _       = []
        triples = concatMap makeTriples tokens

        frequenciesMap = frequenciesOfElementsMap triples -- still slow, but better
        adjustedFrequenciesMap = adjustFrequenciesByHeuristic frequenciesMap isTripleGood

        maxBySnd p1@(_, v1) p2@(_, v2) = if v1 > v2 then p1 else p2
        mostFrequent = Map.foldrWithKey (\k1 n1 (k2, n2) -> maxBySnd (k1, n1) (k2, n2)) ((0, 0, 0), 0) adjustedFrequenciesMap

        isTripleGood :: (TokenID, TokenID, TokenID) -> Bool
        isTripleGood (t1, t2, t3) = hasSeptPrefix && hasSeptSuffix
            where
                potentialToken =  concat [ fromJust $ lookup t1 decodeTable
                                         , fromJust $ lookup t2 decodeTable
                                         , fromJust $ lookup t3 decodeTable
                                         ]

                hasSeptPrefix = any (`isPrefixOf` potentialToken) septs
                hasSeptSuffix = any (`isSuffixOf` potentialToken) septs



addMergedToken :: (TokenID, TokenID, TokenID) -> TokenizerState -> TokenizerState
addMergedToken (t1, t2, t3) tState@(TokenizerState {decodeTable = decodeTable}) = tState {decodeTable = newDecodeTable}
    where
        newTokenID = (+1) $ maximum $ map fst decodeTable
        tokenValue1 = fromJust $ lookup t1 decodeTable
        tokenValue2 = fromJust $ lookup t2 decodeTable
        tokenValue3 = fromJust $ lookup t3 decodeTable
        newDecodeTable = (newTokenID, tokenValue1 ++ tokenValue2 ++ tokenValue3) : decodeTable


mergeTokenTriple :: TokenizerState -> (TokenID, TokenID, TokenID) -> TokenizerState
mergeTokenTriple tState@(TokenizerState {encodedTexts = encodedTexts, decodeTable = decodeTable}) (t1, t2, t3) = newState
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

makeOneToken :: TokenizerState -> TokenizerState
makeOneToken tState = mergedTokenState
    where tokenTriple = mostFrequentTokenTriple tState
          addedTokenState = addMergedToken tokenTriple tState
          mergedTokenState = mergeTokenTriple addedTokenState tokenTriple

makeNTokens :: TokenizerState -> Int -> TokenizerState
makeNTokens tState n
    | n > 0 = makeNTokens (makeOneToken tState) (n-1)
    | otherwise = tState