{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import           System.Console.CmdArgs (Data, Default (def), Typeable, cmdArgs,
                                         help, typFile, (&=))
import           System.Exit
import           TokenCreator
import           TokenReportCreator
import           Data.List.Split
import Data.List (intercalate, nub)
import Songs (contentToChords, chordsToDiff)
import TokenToBlock (tokenToBlock)



songsInRelativeDir :: FilePath
songsInRelativeDir = "./data/relative-notations/"

reportsDir :: FilePath
reportsDir = "./data/reports/"

tokensDir :: FilePath
tokensDir = "./data/tokens/"

knownBlocksDir :: FilePath
knownBlocksDir = "./data/known-blocks/"


data CreatorArgs = CreatorArgs
                   { notation_file         :: FilePath
                   , make_n_tokens         :: Int
                   , report_file           :: FilePath
                   , tokens_file           :: FilePath
            }
            deriving (Show, Read, Data, Typeable)

defaultArgs :: CreatorArgs
defaultArgs = CreatorArgs
              { notation_file = def &= typFile &= help "A name for the input file with songs in relative notation"
              , make_n_tokens = 0 &= help "How many tokens should it create"

              , report_file = def &= typFile &= help "A name for the result file with a report"
              , tokens_file = def &= help "A name for the result file with tokens"
              }

getKnownBlocks :: IO [(String, String)]
getKnownBlocks = do
    contents <- readFile (knownBlocksDir <> "blocks-norm-notation.txt")
    let lined = filter (/= "") $ lines contents
        splited = map (splitOn " : ") lined

        splitToPair :: [String] -> (String, String)
        splitToPair (name:blk:_) = (name,blk)
        splitToPair unexpected = error $ "Unable to split by ':' following line:" ++ intercalate " : " unexpected

        namedBlocks = map splitToPair splited
        noSeparatorsBlocks = map (\(name, blk) ->(name, unwords $ words $ filter (/= '|') blk)) namedBlocks
    pure noSeparatorsBlocks

main :: IO()
main = do
    args <- cmdArgs defaultArgs

    relNotationContents <-  if notation_file args == def
                            then do
                                putStrLn "No relative notation input file specified, abort."
                                exitFailure
                            else do
                                putStrLn $ "Relative notation input file at " <> (songsInRelativeDir <> notation_file args)
                                readFile (songsInRelativeDir <> notation_file args)

    namedBlocks <- getKnownBlocks

    let initTokenCreatorState = textToTokenizerState $ lines relNotationContents
        finalTokenCreatorState = makeNTokens initTokenCreatorState (make_n_tokens args)
        tokens = map snd $ decodeTable finalTokenCreatorState -- more specific tokens on top
        report = createReport finalTokenCreatorState namedBlocks

    if report_file args == def
    then putStrLn "No report file specified, skip."
    else do
        putStrLn $ "Report file at " <> (reportsDir <> report_file args)
        writeFile (reportsDir <> report_file args) report


    if tokens_file args == def
    then putStrLn "No token out file specified, skip."
    else do
        putStrLn $ "Token out file at " <> (tokensDir <> tokens_file args)
        writeFile (tokensDir <> tokens_file args) (unlines tokens)


    let diffs = map (chordsToDiff . contentToChords . snd) namedBlocks
        tokenDictionary = nub $ concat diffs

    putStrLn "Dictionary:"
    print tokenDictionary

    putStrLn "Known blocks:"
    let blocksInDiffs = map (\(name, blk) -> (name, unwords $ chordsToDiff $ contentToChords blk)) namedBlocks

    print blocksInDiffs

    putStrLn "Done"

