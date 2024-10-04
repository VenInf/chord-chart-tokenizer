{-# LANGUAGE ScopedTypeVariables #-}
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit ((@?=), testCase)
import Songs
import System.Directory
import System.FilePath
import Chords
import Data.List (nub, sort)

rawSongsDir :: FilePath
rawSongsDir = "./data/raw-songs/"

tests :: [Song] -> TestTree
tests songs = testGroup "Songs json tests" [ sameAmountOfChordsTests songs
                                           , correctAmountOfDiffsTests songs]
  where
    sameAmountOfChordsTests :: [Song] -> TestTree
    sameAmountOfChordsTests songs = testGroup "Same amounts of chords tests:" (map genTest songs)
      where
        genTest song = testCase ("Same amounts of chords in " <> title song) $ length (chords song) @?= chordsAmount song
        chordsAmount song = length $ filter (/= "NC") $ words $ filter (/= '|') $ content song

    correctAmountOfDiffsTests :: [Song] -> TestTree
    correctAmountOfDiffsTests songs = testGroup "Correct amounts of diffs tests:" (map genTest songs)
      where
        genTest song = testCase ("Correct amounts of diffs in " <> title song) $ length (words $ diffView song) @?= diffsAmount song
        diffsAmount song = 2 * chordsAmount song - 1
        chordsAmount song = length $ filter (/= "NC") $ words $ filter (/= '|') $ content song

chordsReadings :: [Song] -> [(String, String)]
chordsReadings songs = sort $ nub $ zip chordsRaw chordsParsed
  where
    chordsRaw = concatMap (filter (/= "NC") . words . filter (/= '|') . content) songs
    chordsParsed = map showChord $ concatMap chords songs



main :: IO ()
main = do

    songsFileNames <- listDirectory rawSongsDir
    let songsPaths = [rawSongsDir </> name | name <- songsFileNames]
    songsContents <- mapM readFile songsPaths

    let songs = map contentToSong songsContents

    defaultMain $ testGroup "Tests" [tests songs]
