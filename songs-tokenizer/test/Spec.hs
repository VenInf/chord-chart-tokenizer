{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Chords
import           Data.Aeson
import           Data.List        (nub, sort, sortOn)
import           Data.Maybe       (fromJust)
import           Songs
import           SplitByTokens
import           System.Directory
import           System.Exit
import           System.FilePath
import           Test.Tasty       (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))

addTokenView :: Song -> [String] -> [String] -> Song
addTokenView song@(Song{content=cnt}) tokensDictionary tokens = song{tokenView = Just tknView, blockView = Just blkView}
    where
        wordedSong = words cnt
        blkView = chordsByTokens wordedSong tokens
        tknView = map (unwords . chordsToDiff . contentToChords) blkView


tests :: [Song] -> TestTree
tests songs = testGroup "Songs json tests" [ sameAmountDiffViewTests songs
                                           , validTokenization songs
                                           ]
  where
    sameAmountDiffViewTests :: [Song] -> TestTree
    sameAmountDiffViewTests songs = testGroup "Same amounts after tokenizatoin tests:" (map genTest songs)
      where
        genTest song = testCase ("Same amounts of chords in " <> title song) $ diffCorrectedAmount song @?= chordsAmount song
        chordsAmount song = length $ chords song
        diffCorrectedAmount song = (length (words $ diffView song) `div` 2) + 1

    validTokenization :: [Song] -> TestTree
    validTokenization songs = testGroup "Tokens after joining give the original content:" (map genTest songs)
      where
        genTest song = testCase ("Tokenization is valid in " <> title song) $ joinedBlocks song @?= (unwords . words . content) song
        joinedBlocks song = unwords $ fromJust $ blockView song

chordsReadings :: [Song] -> [(String, String)]
chordsReadings songs = sort $ nub $ zip chordsRaw chordsParsed
  where
    chordsRaw = concatMap (filter (/= "NC") . words . filter (/= '|') . content) songs
    chordsParsed = map showChord $ concatMap chords songs


songsJSONDir :: FilePath
songsJSONDir = "./data/out-songs/"

tokensDir :: FilePath
tokensDir = "./data/tokens/"

main :: IO ()
main = do
    mbSongs <- decodeFileStrict (songsJSONDir </> "parsed.json")

    sngs <- case mbSongs of
             Nothing -> do
                        putStrLn "Failed to parce JSON in provided file, abort."
                        exitFailure
             Just sngs -> pure (songs sngs)

    rawTokens <- readFile (tokensDir <> "block-tokens.txt")

    let tokens = lines rawTokens
        tokensDictionary = makeTokensDictionary $ map diffView sngs
        updatedSongs = Songs $ map (\sng -> addTokenView sng tokensDictionary tokens) sngs

    putStrLn $ "Using tokens: " <> rawTokens
    putStrLn $ "Using dictionary: " <> unwords tokensDictionary


    defaultMain $ testGroup "Tests" [tests (songs updatedSongs)]
