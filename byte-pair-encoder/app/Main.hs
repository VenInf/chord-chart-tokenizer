{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Main (main) where

import System.Console.CmdArgs
import Tokenizer
import StateSaver (saveTo)
import Tokenizer (textToTokenizerState, TokenizerState)
import System.Exit 

data BPE = BPE {text_path :: FilePath
               ,config_path :: FilePath
               ,tokenizer_state_path :: FilePath
               
               ,make_tokens :: Int              
               
               ,save_state_path :: FilePath
               ,give_top_n_tokens :: Int
               }
               deriving (Show, Read, Data, Typeable)

defaultArgs = BPE
              {text_path = def &= typFile &= help "Get data for tokenization from file as a text"
              ,config_path = def &= typFile &= help "Specify non-default config file (ignore other flags if using this one)"
              ,tokenizer_state_path = def &= typFile &= help "Specify a tokenizer save file to continue work (loads it if specified)"

              ,make_tokens = 0 &= help "How many tokens should it create"

              ,save_state_path = def &= typFile &= help "If specified, saves the internal state in the path"
              ,give_top_n_tokens = 0 &= help "Gives top n tokens as an output"
              }

loadBPE :: FilePath -> IO BPE
loadBPE configurationPath = do
  bpeString <- readFile configurationPath
  return (read bpeString :: BPE)

loadTokenizerState :: FilePath -> IO TokenizerState
loadTokenizerState filePath = do
    stateString <- readFile filePath
    return (read stateString :: TokenizerState)

-- loadConfigCase :: FilePath -> IO()

handleTokenizer :: TokenizerState -> BPE -> IO()
handleTokenizer tokenizerState args = do
  let newTokenizerState = makeNTokens tokenizerState (make_tokens args)
  if save_state_path args /= def
    then saveTo newTokenizerState (save_state_path args) 
    else if give_top_n_tokens args /= 0
         then print $ humanReadebleRankings (topNTokens newTokenizerState (give_top_n_tokens args)) newTokenizerState
         else do putStrLn "Finished working"
                 exitSuccess

handlePathArgs :: BPE -> IO()
handlePathArgs args = do
  if config_path args /= def
    then do putStrLn "Not implemented"
            exitFailure -- loadConfigCase (config_path args)
    else if save_state_path args /= def
         then do tokenizerState <- loadTokenizerState (save_state_path args)
                 handleTokenizer tokenizerState args
         else if text_path args /= def
              then do text <- readFile (text_path args)
                      let tokenizerState = textToTokenizerState text
                      handleTokenizer tokenizerState args
              else do putStrLn "Both text_path and save_state_path specified, fail"
                      exitFailure


main :: IO()
main = do
  print "before"
  args <- cmdArgs defaultArgs
  print args
  
  handlePathArgs args


  -- print "flush args"


  -- print "load from config"
  -- args <- loadBPE
  -- print args

  -- print "Done"

  -- inputList <- readFile (text_path args)
  -- let tState = textToTokenizerState inputList
  -- let resultState = makeNTokens tState (make_tokens args)
  -- print $ humanReadebleRankings (topNTokens resultState (top_n_tokens args)) resultState

