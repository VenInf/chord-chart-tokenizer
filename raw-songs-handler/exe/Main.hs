{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import           Data.Aeson
import           Songs
import           System.Console.CmdArgs (Data, Default (def), Typeable, cmdArgs,
                                         help, typFile, (&=))
import           System.Directory
import           System.FilePath

rawSongsDir :: FilePath
rawSongsDir = "./data/raw-songs/"

songsJSONDir :: FilePath
songsJSONDir = "./data/out-songs/"

songsInRelativeDir :: FilePath
songsInRelativeDir = "./data/relative-notations/"

data RawSongsHandler = RawSongsHandler
                       { out_json_file     :: FilePath
                       , rel_notation_file :: FilePath
                       }
                       deriving (Show, Read, Data, Typeable)

defaultArgs :: RawSongsHandler
defaultArgs = RawSongsHandler
              { out_json_file = def &= typFile &= help "A name for the result JSON file with all songs parsed"
              , rel_notation_file = def &= typFile &= help "A name for the result file with songs in the relative notation"
              }

main :: IO()
main = do
    args <- cmdArgs defaultArgs

    songsFileNames <- listDirectory rawSongsDir
    let songsPaths = [rawSongsDir </> name | name <- songsFileNames]
    songsContents <- mapM readFile songsPaths

    let sngs = map contentToSong songsContents
        diffs = map diffView sngs

    if out_json_file args == def
    then putStrLn "No JSON out file specified, skip."
    else do
        putStrLn $ "JSON out file at " <> songsJSONDir <> out_json_file args
        encodeFile (songsJSONDir <> out_json_file args) (Songs sngs)

    if rel_notation_file args == def
    then putStrLn "No relative notation out file specified, skip."
    else do
        putStrLn $ "relative notation out file at " <> songsInRelativeDir <> rel_notation_file args
        writeFile (songsInRelativeDir <> rel_notation_file args) (unlines diffs)

    putStrLn "Done"
