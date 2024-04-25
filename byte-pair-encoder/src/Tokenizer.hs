{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Tokenizer
    (textToSimpleTokens
    ,mostFrequentTokenPair
    ,addMergedToken
    ,makeOneToken
    ,makeNTokens
    ,tokensToText
    ) where

import Data.List (nub, group, sort, maximumBy)
import qualified Data.Map.Strict as Map
import GHC.Exception (underflowException)
import Data.Function (on)
import Data.Maybe (fromJust)

type Text = String
type TokenID = Int

toFst :: (b -> a) -> b -> (a, b)
toFst f a = (f a, a)

fmapToFst :: Functor f => (a -> b) -> f a -> f (b, a)
fmapToFst = fmap . toFst

textToSimpleTokens :: Text -> ([TokenID], [(TokenID, String)])
textToSimpleTokens txt = (encodedTxt, decodeTable)
    where 
          encodedTxt = map fromEnum txt
          charsDecodeTable = fmapToFst fromEnum (nub txt)
          decodeTable = map (\(a, b) -> (a, [b])) charsDecodeTable
        
tokensToText :: ([TokenID], [(TokenID, String)]) -> Text
tokensToText (encodedTxt, decodeTable) = unwords $ map (\t ->  fromJust $ lookup t decodeTable) encodedTxt
 

mostFrequentTokenPair :: [TokenID] -> (TokenID, TokenID)
mostFrequentTokenPair tokens = fst mostFrequent
    where
          makePairs (a:b:t) = (a, b) : makePairs (b:t)
          makePairs _ = []
          pairs = makePairs tokens
          
          frequenciesMap = foldr (\pair counterMap -> Map.insertWith (+) pair 1 counterMap ) Map.empty pairs -- still slow, but better
          maxBySnd p1@(k1, v1) p2@(k2, v2) = if v1 > v2 then p1 else p2
          mostFrequent = Map.foldrWithKey (\k1 n1 (k2, n2) -> maxBySnd (k1, n1) (k2, n2)) ((0, 0), 0) frequenciesMap


addMergedToken :: (TokenID, TokenID) -> [(TokenID, String)] -> [(TokenID, String)]
addMergedToken pair@(t1, t2) decodeTable = (newTokenID, tokenValue1 ++ tokenValue2) : decodeTable 
    where newTokenID = (+1) $ maximum $ map fst decodeTable
          tokenValue1 = fromJust $ lookup t1 decodeTable
          tokenValue2 = fromJust $ lookup t2 decodeTable

tokenByValue :: String -> [(TokenID, String)] -> TokenID -- second value lookup
tokenByValue value decodeTable = fst $ head $ filter (\(_, v) -> v == value) decodeTable 

mergeTokenPair :: [TokenID] -> [(TokenID, String)] -> (TokenID, TokenID) -> [TokenID]
mergeTokenPair encodedTxt decodeTable (t1, t2) = replacePairs encodedTxt
    where tokenValue1 = fromJust $ lookup t1 decodeTable
          tokenValue2 = fromJust $ lookup t2 decodeTable
          newTokenValue = tokenValue1 ++ tokenValue2
          newTokenID = tokenByValue newTokenValue decodeTable
          
          replacePairs (a:b:t)
            | a == t1 && b == t2 = newTokenID : replacePairs t
            | otherwise = a : replacePairs (b:t)
          replacePairs [b] = [b]
          replacePairs [] = []

makeOneToken :: ([TokenID], [(TokenID, String)]) -> ([TokenID], [(TokenID, String)])
makeOneToken (tokens, decodeTable) = (newTokens, newTable)
    where tokenPair = mostFrequentTokenPair tokens
          newTable = addMergedToken tokenPair decodeTable
          newTokens = mergeTokenPair tokens newTable tokenPair

makeNTokens :: ([TokenID], [(TokenID, String)]) -> Int -> ([TokenID], [(TokenID, String)])
makeNTokens state n
    | n > 0 = makeNTokens (makeOneToken state) (n-1)
    | otherwise = state


-- main = do
--     let txt = "aaabdaaabac"
--     let (tokens, table) = textToSimpleTokens txt -- [1,1,1,2,3,1,1,1,2,1,4] {1: "a", 2: "b", 4: "c", 3: "d"} 
    
--     let tokenPair = mostFrequentTokenPair tokens -- (1,1)
--     let table = addMergedToken tokens table -- {1: "a", 2: "b", 4: "c", 3: "d", 5 : "aa"}
--     let tokens = mergeTokenPair tokens table tokenPair -- [5,1,2,3,5,1,2,1,4]

--     return

