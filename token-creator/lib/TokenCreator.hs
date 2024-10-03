{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE RecordWildCards #-}

module TokenCreator where

import           Data.Function   (on)
import           Data.Hashable   (hash)
import           Data.List       (intercalate, sortBy, sortOn, nub)
import           Data.List.Extra (splitOn)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust)
import Data.List (isPrefixOf, isSuffixOf)
import Chords

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
frequenciesOfElementsMap element = foldr (\elmnt counterMap -> Map.insertWith (+) elmnt 1 counterMap ) Map.empty element

tokenByValue :: String -> [(TokenID, String)] -> TokenID -- second value lookup
tokenByValue value decodeTable = fst $ head $ filter (\(_, v) -> v == value) decodeTable

topTokens :: TokenCreatorState -> [(TokenID, Int)]
topTokens TokenCreatorState {encodedTexts = encodedTexts} = rankings
    where frequencies = frequenciesOfElementsMap $ concat encodedTexts
          rankings = sortBy (flip compare `on` snd) (Map.toList frequencies)

humanReadebleRankings :: [(TokenID, Int)] -> TokenCreatorState -> [(String, Int)]
humanReadebleRankings rankings (TokenCreatorState {decodeTable = decodeTable}) = map (\(rID,rs) -> (fromJust $ lookup rID decodeTable, rs)) rankings


adjustFrequenciesByHeuristic :: (Ord k, Num a) => Map.Map k a -> (k -> Bool) -> Map.Map k a
adjustFrequenciesByHeuristic frequencies isGood = Map.mapWithKey (\key freq -> if isGood key then freq + adjustDiff else freq) frequencies
    where
        adjustDiff = fromIntegral $ length frequencies

mostFrequentTokenTriple :: TokenCreatorState -> (TokenID, TokenID, TokenID)
mostFrequentTokenTriple (TokenCreatorState {encodedTexts = tokens, decodeTable = decodeTable}) = fst mostFrequent
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

makeOneToken :: TokenCreatorState -> TokenCreatorState
makeOneToken tState = mergedTokenState
    where tokenTriple = mostFrequentTokenTriple tState
          addedTokenState = addMergedToken tokenTriple tState
          mergedTokenState = mergeTokenTriple addedTokenState tokenTriple

makeNTokens :: TokenCreatorState -> Int -> TokenCreatorState
makeNTokens tState n
    | n > 0 = makeNTokens (makeOneToken tState) (n-1)
    | otherwise = tState