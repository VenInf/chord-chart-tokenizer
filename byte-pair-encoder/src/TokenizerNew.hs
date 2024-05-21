{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module TokenizerNew
    (textToTokenizerState
    ,mostFrequentTokenPair
    ,addMergedToken
    ,makeOneToken
    -- ,makeNTokens
    ,tokenizerStateToText
    -- ,topNTokens
    -- ,humanReadebleRankings
    ) where

import qualified Data.Map.Strict as Map
import GHC.Exception (underflowException)
import Data.Function (on)
import Data.List (nub, group, sort, maximumBy, sortBy)
import Data.Maybe (fromJust)

type Text = String
type TokenID = Int

data TokenizerState = TokenizerState {text :: String,
                                      encodedText :: [TokenID],
                                      decodeTable :: [(TokenID, String)]
                                      } deriving (Show)

toFst :: (b -> a) -> b -> (a, b)
toFst f a = (f a, a)

fmapToFst :: Functor f => (a -> b) -> f a -> f (b, a)
fmapToFst = fmap . toFst

textToTokenizerState :: Text -> TokenizerState
textToTokenizerState text = TokenizerState {text = text, encodedText = encodedTxt, decodeTable = decodeTable}
    where 
          encodedTxt = map fromEnum text
          charsDecodeTable = fmapToFst fromEnum (nub text)
          decodeTable = map (\(a, b) -> (a, [b])) charsDecodeTable

tokenizerStateToText :: TokenizerState -> Text
tokenizerStateToText (TokenizerState {encodedText = encodedText, decodeTable = decodeTable}) = text
    where text = unwords $ map (\t ->  fromJust $ lookup t decodeTable) encodedText

frequenciesOfElementsMap :: (Foldable t, Ord k, Num a) => t k -> Map.Map k a
frequenciesOfElementsMap element = foldr (\element counterMap -> Map.insertWith (+) element 1 counterMap ) Map.empty element

mostFrequentTokenPair :: TokenizerState -> (TokenID, TokenID)
mostFrequentTokenPair (TokenizerState {encodedText = tokens}) = fst mostFrequent
    where
          makePairs (a:b:t) = (a, b) : makePairs (b:t)
          makePairs _ = []
          pairs = makePairs tokens
          
          frequenciesMap = frequenciesOfElementsMap pairs -- still slow, but better
          maxBySnd p1@(k1, v1) p2@(k2, v2) = if v1 > v2 then p1 else p2
          mostFrequent = Map.foldrWithKey (\k1 n1 (k2, n2) -> maxBySnd (k1, n1) (k2, n2)) ((0, 0), 0) frequenciesMap

addMergedToken :: (TokenID, TokenID) -> TokenizerState -> TokenizerState
addMergedToken pair@(t1, t2) tState@(TokenizerState {decodeTable = decodeTable}) = tState {decodeTable = newDecodeTable}
    where newTokenID = (+1) $ maximum $ map fst decodeTable
          tokenValue1 = fromJust $ lookup t1 decodeTable
          tokenValue2 = fromJust $ lookup t2 decodeTable
          newDecodeTable = (newTokenID, tokenValue1 ++ tokenValue2) : decodeTable

tokenByValue :: String -> [(TokenID, String)] -> TokenID -- second value lookup
tokenByValue value decodeTable = fst $ head $ filter (\(_, v) -> v == value) decodeTable 

mergeTokenPair :: TokenizerState -> (TokenID, TokenID) -> TokenizerState
mergeTokenPair tState@(TokenizerState {encodedText = encodedText, decodeTable = decodeTable}) (t1, t2) = newState
    where tokenValue1 = fromJust $ lookup t1 decodeTable
          tokenValue2 = fromJust $ lookup t2 decodeTable
          newTokenValue = tokenValue1 ++ tokenValue2
          newTokenID = tokenByValue newTokenValue decodeTable
          
          replacePairs (a:b:t)
            | a == t1 && b == t2 = newTokenID : replacePairs t
            | otherwise = a : replacePairs (b:t)
          replacePairs [b] = [b]
          replacePairs [] = []

          newState = tState {encodedText = replacePairs encodedText}

makeOneToken :: TokenizerState -> TokenizerState
makeOneToken tState@(TokenizerState {encodedText = tokens, decodeTable = decodeTable}) = mergedTokenState
    where tokenPair = mostFrequentTokenPair tState
          addedTokenState = addMergedToken tokenPair tState
          mergedTokenState = mergeTokenPair addedTokenState tokenPair

---------------------

-- makeNTokens :: ([TokenID], [(TokenID, String)]) -> Int -> ([TokenID], [(TokenID, String)])
makeNTokens :: TokenizerState -> Int -> TokenizerState
makeNTokens tState n
    | n > 0 = makeNTokens (makeOneToken tState) (n-1)
    | otherwise = tState

topNTokens :: TokenizerState -> Int -> [(TokenID, Int)]
topNTokens TokenizerState {encodedText = tokens, decodeTable = decodeTable} n = take n rankings
    where frequencies = frequenciesOfElementsMap tokens
          rankings = sortBy (flip compare `on` snd) (Map.toList frequencies)

humanReadebleRankings :: [(TokenID, Int)] -> [(TokenID, String)] -> [(String, Int)]
humanReadebleRankings rankings decodeTable = map (\(rID,rs) -> (fromJust $ lookup rID decodeTable, rs)) rankings


