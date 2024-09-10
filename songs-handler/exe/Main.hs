{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import System.Directory
import System.FilePath
import GHC.Generics
import Data.Aeson
import Data.List.Split
import Data.Char

import Debug.Trace

songsDir :: FilePath
songsDir = "./data"


data Song = Song {
      title :: String
    , composedBy :: String
    , dbKeySig :: String
    , timeSig :: String
    , bars :: String
    , content :: String
    , diffView :: String
    } deriving (Generic, Show, ToJSON, FromJSON)

newtype Songs
  = Songs {songs :: [Song]}
  deriving (Generic, Show, ToJSON, FromJSON)

showDiff :: Int -> String
showDiff n = if n > 0
             then "(+" ++ show n ++ ")"
             else "(" ++ show n ++ ")"

makeContentRelative :: String -> String
makeContentRelative content = unwords $ joinRaw relativeRaw
    where
        unbared = filter (/= '|') content
        cords = splitOn " " $ unwords $ words unbared
        cordPairs = makePairs cords
        relativeRaw = map twoCordsToRelative cordPairs

        makePairs (a:b:t) = (a, b) : makePairs (b:t)
        makePairs _       = []

        joinRaw :: [(String, String, String)] -> [String]
        joinRaw [(sept1, df1, common1), (common2, df2, sept2)] =
            if common1 == common2
            then [sept1, df1, common1, df2, sept2]
            else error $ "common 1 (" ++ show common1 ++ ") and common 2 (" ++ show common2 ++ "aren't equal"
        joinRaw ((sept1, df1, common1):(common2, df2, sept2):rest) =
            if common1 == common2
            then [sept1, df1] ++ joinRaw ((common2, df2, sept2):rest)
            else error $ "common 1 (" ++ show common1 ++ ") and common 2 (" ++ show common2 ++ "aren't equal"
        joinRaw _ = []

twoCordsToRelative :: (String, String) -> (String, String, String)
twoCordsToRelative (cord1@(note1:sept1), cord2@(note2:sept2)) = (sept1, showDiff $ ord note2 - ord note1, sept2)


contentToSong :: String -> Song
contentToSong content = go $ lines content
  where
    rmBeforeEqSign :: String -> String
    rmBeforeEqSign l = splitOn " = " l !! 1

    go :: [String] -> Song
    go (titleL:composedByL:dbKeySigL:timeSigL:barsL:contentL) = Song {..}
      where
        title = rmBeforeEqSign titleL
        composedBy = rmBeforeEqSign composedByL
        dbKeySig = rmBeforeEqSign dbKeySigL
        timeSig = rmBeforeEqSign timeSigL
        bars = rmBeforeEqSign barsL
        content = filter (/= '\n') $ unlines contentL
        diffView = makeContentRelative content
    go _ = error "unexpected number of lines"


main :: IO()
main = do
    songsFileNames <- listDirectory songsDir
    let songsPaths = [songsDir </> name | name <- songsFileNames]
    songsContents <- mapM readFile songsPaths

    let songs = map contentToSong songsContents

    encodeFile "./out/allSongs.json" (Songs songs)

    putStrLn "Done"
