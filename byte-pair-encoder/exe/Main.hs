{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Main (main) where

import           StateSaver             (saveTo)
import           System.Console.CmdArgs (Data, Default (def), Typeable, cmdArgs,
                                         help, typFile, (&=))
import           System.Exit
import           Tokenizer
import Colonnade

data BPE = BPE { text_path         :: FilePath
               , dict_path         :: FilePath
               , config_path       :: FilePath
               , load_state_path   :: FilePath

               , make_tokens       :: Int

               , save_state_path   :: FilePath
               , give_top_n_tokens :: Int
               }
               deriving (Show, Read, Data, Typeable)

defaultArgs :: BPE
defaultArgs = BPE
              { text_path = def &= typFile &= help "Get data for tokenization from file as a text"
              , dict_path = def &= typFile &= help "Token's dictionary file, each token on a separate line, higher have a priority"
              , config_path = def &= typFile &= help "Specify non-default config file (ignore other flags if using this one)"
              , load_state_path = def &= typFile &= help "Specify a tokenizer save file to continue work (loads it if specified)"

              , make_tokens = 0 &= help "How many tokens should it create"

              , save_state_path = def &= typFile &= help "If specified, saves the internal state in the path"
              , give_top_n_tokens = 0 &= help "Gives top n tokens as an output"
              }


colBPE :: Colonnade Headed BPE String
colBPE = mconcat
        [ headed "Text" text_path
        , headed "Dictionary" dict_path
        , headed "Config" config_path
        , headed "Load state" load_state_path
        , headed "Make n tokens" (show . make_tokens)
        , headed "Save to" save_state_path
        , headed "Show n tokens" (show . give_top_n_tokens)
        ]

data Ranks = Ranks { place :: Int
                   , token :: String
                   , amount :: Int
                   } deriving (Show)

pairToRanks :: [(String, Int)] -> [Ranks]
pairToRanks tokenAmounts = zipWith ($) ranksWithPlaces tokenAmounts
    where
        ranksWithPlaces = [uncurry (Ranks p) | p <- [1..]]

colRanks :: Colonnade Headed Ranks String
colRanks = mconcat
        [ headed "Place" (show . place)
        , headed "Token" token
        , headed "Amount" (show . amount)
        ]

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
         then do putStrLn "Top n tokens with the respective frequencies"
                 let tokenAmounts = humanReadebleRankings (topNTokens newTokenizerState (give_top_n_tokens args)) newTokenizerState
                 putStrLn $ ascii colRanks $ pairToRanks tokenAmounts
         else do putStrLn "No give_top_n_tokens or save_state_path specified, nothing to do"
                 exitSuccess

handlePathArgs :: BPE -> IO()
handlePathArgs args = do
  if config_path args /= def
    then do putStrLn "Not implemented" -- loadConfigCase (config_path args)
            exitFailure
    else if load_state_path args /= def
         then do tokenizerState <- loadTokenizerState (load_state_path args)
                 handleTokenizer tokenizerState args
         else if text_path args == def
              then do putStrLn "No text_path or load_state_path specified, fail"
                      exitFailure
              else if dict_path args == def
                   then do txt <- readFile (text_path args)
                           let tokenizerState = textToTokenizerStateWithDict (lines txt) []
                           handleTokenizer tokenizerState args
                   else do txt <- readFile (text_path args)
                           dict <- readFile (dict_path args)
                           let tokenizerState = textToTokenizerStateWithDict (lines txt) (lines dict)
                           handleTokenizer tokenizerState args



main :: IO()
main = do
  args <- cmdArgs defaultArgs
  putStrLn "Using following arguments:"
  putStrLn (ascii colBPE [args])

  handlePathArgs args
  putStrLn "Done"
