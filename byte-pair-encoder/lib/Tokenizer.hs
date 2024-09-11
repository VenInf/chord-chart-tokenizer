{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE RecordWildCards #-}

module Tokenizer where

import           Data.Function   (on)
import           Data.Hashable   (hash)
import           Data.List       (intercalate, nub, sortBy, sortOn)
import           Data.List.Extra (splitOn)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust)

type TokenID = Int

data TokenizerState = TokenizerState { texts        :: [String]
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

textToTokenizerStateWithDict :: [String] -> [String] -> TokenizerState
textToTokenizerStateWithDict txts dict = TokenizerState {texts = txts, encodedTexts = encodedTxts, decodeTable = decodeTable}
    where
        filteredTxt = filter (`notElem` dict) (map (:[]) (nub $ concat txts))
        decodeTable = sortOn (\(_, b) -> negate $ length b) $ fmapToFst hash (dict <> filteredTxt)
        encodedTxts = map (`encode` decodeTable) txts


tokenizerStateToTexts :: TokenizerState -> [String]
tokenizerStateToTexts (TokenizerState {..}) = map decodeText encodedTexts
    where
        decodeText :: [TokenID] -> String
        decodeText encTxt = unwords $ map (\t ->  fromJust $ lookup t decodeTable) encTxt

frequenciesOfElementsMap :: (Foldable t, Ord k, Num a) => t k -> Map.Map k a
frequenciesOfElementsMap element = foldr (\elmnt counterMap -> Map.insertWith (+) elmnt 1 counterMap ) Map.empty element

mostFrequentTokenPair :: TokenizerState -> (TokenID, TokenID)
mostFrequentTokenPair (TokenizerState {encodedTexts = tokens}) = fst mostFrequent
    where
          makePairs (a:b:t) = (a, b) : makePairs (b:t)
          makePairs _       = []
          pairs = concatMap makePairs tokens

          frequenciesMap = frequenciesOfElementsMap pairs -- still slow, but better
          maxBySnd p1@(_, v1) p2@(_, v2) = if v1 > v2 then p1 else p2
          mostFrequent = Map.foldrWithKey (\k1 n1 (k2, n2) -> maxBySnd (k1, n1) (k2, n2)) ((0, 0), 0) frequenciesMap

addMergedToken :: (TokenID, TokenID) -> TokenizerState -> TokenizerState
addMergedToken (t1, t2) tState@(TokenizerState {decodeTable = decodeTable}) = tState {decodeTable = newDecodeTable}
    where newTokenID = (+1) $ maximum $ map fst decodeTable
          tokenValue1 = fromJust $ lookup t1 decodeTable
          tokenValue2 = fromJust $ lookup t2 decodeTable
          newDecodeTable = (newTokenID, tokenValue1 ++ tokenValue2) : decodeTable

tokenByValue :: String -> [(TokenID, String)] -> TokenID -- second value lookup
tokenByValue value decodeTable = fst $ head $ filter (\(_, v) -> v == value) decodeTable

mergeTokenPair :: TokenizerState -> (TokenID, TokenID) -> TokenizerState
mergeTokenPair tState@(TokenizerState {encodedTexts = encodedTexts, decodeTable = decodeTable}) (t1, t2) = newState
    where tokenValue1 = fromJust $ lookup t1 decodeTable
          tokenValue2 = fromJust $ lookup t2 decodeTable
          newTokenValue = tokenValue1 ++ tokenValue2
          newTokenID = tokenByValue newTokenValue decodeTable

          replacePairs (a:b:t)
            | a == t1 && b == t2 = newTokenID : replacePairs t
            | otherwise = a : replacePairs (b:t)
          replacePairs [b] = [b]
          replacePairs [] = []

          newState = tState {encodedTexts = map replacePairs encodedTexts}

makeOneToken :: TokenizerState -> TokenizerState
makeOneToken tState = mergedTokenState
    where tokenPair = mostFrequentTokenPair tState
          addedTokenState = addMergedToken tokenPair tState
          mergedTokenState = mergeTokenPair addedTokenState tokenPair

makeNTokens :: TokenizerState -> Int -> TokenizerState
makeNTokens tState n
    | n > 0 = makeNTokens (makeOneToken tState) (n-1)
    | otherwise = tState

topNTokens :: TokenizerState -> Int -> [(TokenID, Int)]
topNTokens TokenizerState {encodedTexts = encodedTexts} n = take n rankings
    where frequencies = frequenciesOfElementsMap $ concat encodedTexts
          rankings = sortBy (flip compare `on` snd) (Map.toList frequencies)

humanReadebleRankings :: [(TokenID, Int)] -> TokenizerState -> [(String, Int)]
humanReadebleRankings rankings (TokenizerState {decodeTable = decodeTable}) = map (\(rID,rs) -> (fromJust $ lookup rID decodeTable, rs)) rankings


