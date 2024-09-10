{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import System.Directory
import System.FilePath
import GHC.Generics
import Data.Aeson
import Data.List.Split
import Data.Char
import Data.Maybe

import Debug.Trace

jsonFile :: FilePath
jsonFile = "./out/allSongs.json"

data Song = Song {
      title :: String
    , composedBy :: String
    , dbKeySig :: String
    , timeSig :: String
    , bars :: String
    , content :: String
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


main :: IO()
main = do
    (mSongs :: Maybe Songs) <- decodeFileStrict' jsonFile
    let sList = songs $ fromJust mSongs
        sng = head sList
        cnt = content sng

    putStrLn $ title sng
    putStrLn $ makeContentRelative cnt
    putStrLn "Done"
