{-# LANGUAGE ScopedTypeVariables #-}
module TokenReportCreator where

import           Chords
import           Colonnade
import           Data.Function          (on)
import           Data.List              (maximumBy, sortBy, zipWith7)
import           Data.List.Split
import qualified Data.Maybe             as MB
import           Data.Tuple
import           Data.Tuple.Extra       (uncurry3)
import           Songs                  (chordsToDiff, contentToChords)
import           SplitByTokens          (makeTokensDictionary)
import           Text.PrettyPrint.Boxes
import           TokenCreator
import           TokenToBlock



data Ranks = Ranks { place       :: Int
                   , amount      :: Int
                   , probability :: Float
                   , token       :: String
                   , block       :: String
                   , knownName   :: String
                   , howSimilar  :: Float
                   } deriving (Show)

data LenStats = LenStats { lengthLen :: Int
                         , tokenLen  :: String
                         , blockLen  :: String
                         } deriving (Show)

bestMatch :: String -> [String] -> (String, Float)
bestMatch targetBlock knownBlocks = if all isChord blockChords
                                    then (unwords $ map showChord foundMatch, match blockChords foundMatch)
                                    else ("", 0)
    where
        toChords :: String -> [Chord]
        toChords blk = map rawToChord $ splitOn " " $ unwords $ words blk

        match :: [Chord] -> [Chord] -> Float
        match chords1 chords2 = sum (zipWith similarity chords1 chords2)/ fromIntegral (max (length chords1) (length chords2))

        blockChords = toChords targetBlock
        knownBlocksChords = filter (all isChord) $ map toChords knownBlocks

        foundMatch = maximumBy (compare `on` match blockChords) knownBlocksChords


makeRanks :: TokenCreatorState -> [(String, String)] -> [Ranks]
makeRanks state namedBlocks = zipWith7 Ranks [1..] amounts probabilities tokens blocks formattedKnownNames formattedHowSimilar
    where
        normNamedBlocks = map (\(name, blk) -> (name,
                             tokenToBlock tokensDictionary $ unwords $ chordsToDiff $ contentToChords blk)) namedBlocks

        tokenAmounts = humanReadebleRankings (topTokens state) state
        tokensDictionary = makeTokensDictionary (texts state)

        amounts = map snd tokenAmounts
        totalAmount = toEnum $ sum amounts
        probabilities = map ((/ totalAmount) . toEnum) amounts
        tokens = map fst tokenAmounts
        blocks = map (tokenToBlock tokensDictionary) tokens

        -- TODO:
        -- Instead of perfect match normNamedBlocks to blocks
        -- do a fuzzy match
        bestMatches = map (\target -> bestMatch target $ map snd normNamedBlocks) blocks
        knownNames = map ((\blk -> lookup blk (map swap normNamedBlocks)) . fst) bestMatches
        formattedKnownNames = map (MB.fromMaybe "") knownNames
        formattedHowSimilar = map snd bestMatches



makeLenStats :: TokenCreatorState -> [LenStats]
makeLenStats state = map (uncurry3 LenStats) triples
    where
        tokenAmounts = humanReadebleRankings (topTokens state) state
        tokensDictionary = makeTokensDictionary (texts state)

        tokens = map fst tokenAmounts
        blocks = map (tokenToBlock tokensDictionary) tokens
        lengths = map (length . words) blocks

        triples = sortBy (flip (\(a,_, _) (b,_, _) -> compare a b)) (zip3 lengths tokens blocks)

colRanks :: Colonnade Headed Ranks String
colRanks = mconcat
        [ headed "Place" (show . place)
        , headed "Amount" (show . amount)
        , headed "%" (show . (* 100) . probability)
        , headed "Token" token
        , headed "Block" block
        , headed "Closest known" knownName
        , headed "How similar" $ (\score -> if score == 0 then "" else show score) . howSimilar
        ]

colLenStats :: Colonnade Headed LenStats String
colLenStats = mconcat
        [ headed "Length" (show . lengthLen)
        , headed "Token" tokenLen
        , headed "Block" blockLen
        ]

makeStats :: TokenCreatorState -> [Ranks] -> [(String, String)] -> Box
makeStats state ranks knownBlocks = stats
    where
        precentBool = zip (map probability ranks) (map ((> 1) . length . words . block) ranks)
        meaningfulPercent = (* 100) $ sum $ map fst $ filter snd precentBool

        shannonInformation = (-1) * sum [p * logBase 2 p | p <- map probability ranks]

        similarities = map howSimilar ranks
        totalSimilarity = sum similarities / fromIntegral (length similarities)

        stats = vcat top [ hcat top $ map text ["% of covered by meaningful blocks ", show meaningfulPercent]
                         , hcat top $ map text ["information/entropy by Shannon ", show shannonInformation]
                         , hcat top $ map text ["total similarity to known bloks is ", show totalSimilarity]
                         ]

createReport :: TokenCreatorState -> [(String, String)] -> String
createReport state namedBlocks = report
    where
        ranks = ascii colRanks $ makeRanks state namedBlocks
        lenStats = ascii colLenStats $ take 20 $ makeLenStats state
        stats = render $ makeStats state (makeRanks state namedBlocks) namedBlocks
        report = unlines [ "Information about found tokens:"
                         , stats
                        --  , lenStats
                         , ranks ]
