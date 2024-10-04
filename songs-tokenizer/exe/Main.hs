{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Aeson
import           Data.List
import           Data.List.Split
import           GHC.Generics
import           System.Directory
import           System.FilePath
import Chords
import Songs
import SplitByTokens
import           System.Console.CmdArgs (Data, Default (def), Typeable, cmdArgs,
                                         help, typFile, (&=))
import           System.Exit
import SongsReportCreator

songsJSONDir :: FilePath
songsJSONDir = "./data/out-songs/"

tokensDir :: FilePath
tokensDir = "./data/tokens/"

reportsDir :: FilePath
reportsDir = "./data/reports/"

data TokenizerArgs = TokenizerArgs
                   { in_json_file    :: FilePath
                   , tokens_file     :: FilePath
                   , report_file     :: FilePath
                   , out_json_file   :: FilePath
                   }
                     deriving (Show, Read, Data, Typeable)

defaultArgs :: TokenizerArgs
defaultArgs = TokenizerArgs
              { in_json_file = def &= typFile &= help "A name for the input json file with songs"
              , tokens_file = def &= help "A name for the input file with tokens"

              , report_file = def &= help "A name for the result file with a report"
              , out_json_file = def &= typFile &= help "A name for the output json file with songs, incliding tokenized view"
              }

addTokenView :: Song -> [String] -> [String] -> Song
addTokenView song@(Song{content=content}) tokensDictionary tokens = song{tokenView = Just tokenView, blockView = Just blockView}
    where
        wordedSong = words content
        blockView = chordsByTokens wordedSong tokensDictionary tokens
        tokenView = map (unwords . chordsToDiff . contentToChords) blockView

main :: IO()
main = do
    args <- cmdArgs defaultArgs

    (mbSongs :: Maybe Songs) <- if in_json_file args == def
                                then do
                                    putStrLn "No input JSON file with songs specified, abort."
                                    exitFailure
                                else do
                                    putStrLn $ "Input JSON file with songs at " <> (songsJSONDir <> in_json_file args)
                                    decodeFileStrict (songsJSONDir <> in_json_file args)

    songs <- case mbSongs of
             Nothing -> do
                        putStrLn "Failed to parce JSON in provided file, abort."
                        exitFailure
             Just sngs -> pure (songs sngs)

    rawTokens <- if tokens_file args == def
                 then do
                     putStrLn "No input tokens file specified, abort."
                     exitFailure
                 else do
                     putStrLn $ "Tokens input file at " <> (tokensDir <> tokens_file args)
                     readFile (tokensDir <> tokens_file args)


    let tokens = lines rawTokens
        tokensDictionary = nub $ concatMap (words . diffView) songs
        updatedSongs = Songs $ map (\sng -> addTokenView sng tokensDictionary tokens) songs
        report = createReport updatedSongs


    if report_file args == def
    then putStrLn "No report out file specified, skip."
    else do
        putStrLn $ "Report out file at " <> (reportsDir <> report_file args)
        writeFile (reportsDir <> report_file args) report

    if out_json_file args == def
    then putStrLn "No JSON out file specified, skip."
    else do
        putStrLn $ "JSON out file at " <> (songsJSONDir <> out_json_file args)
        encodeFile (songsJSONDir <> out_json_file args) updatedSongs

    putStrLn "Done"