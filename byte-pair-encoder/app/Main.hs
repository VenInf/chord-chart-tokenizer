{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Main (main) where

import System.Console.CmdArgs
import Tokenizer
import StateSaver (saveTo)
import Tokenizer (textToTokenizerState)

configurationPath :: FilePath
configurationPath = "./bpe_config.txt"

data BPE = BPE {dict_file :: FilePath
                ,make_tokens :: Int
                ,top_n_tokens :: Int
                }
                deriving (Show, Read, Data, Typeable)

defaultArgs = BPE{dict_file = def
                  ,make_tokens = 0
                  ,top_n_tokens = 0
                  }

saveBPE :: BPE -> IO () 
saveBPE bpe = saveTo bpe configurationPath

loadBPE :: IO BPE
loadBPE = do
  bpeString <- readFile configurationPath
  return (read bpeString :: BPE)

loadTokenizerState :: FilePath -> IO TokenizerState
loadTokenizerState filePath = do
    stateString <- readFile filePath
    return (read stateString :: TokenizerState)

main :: IO()
main = do
  print "before"
  args <- cmdArgs defaultArgs
  print args
  
  print "flush args"


  print "load from config"
  args <- loadBPE
  print args

  print "Done"
  -- inputList <- readFile (dict_file args)
  -- let tState = textToTokenizerState inputList
  -- let resultState = makeNTokens tState (make_tokens args)
  -- print $ humanReadebleRankings (topNTokens resultState (top_n_tokens args)) resultState

