{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main (main) where

import System.Console.CmdArgs
import Tokenizer
import Tokenizer (humanReadebleRankings, topNTokens)

data BPE = BPE {dict_file :: FilePath
                ,make_tokens :: Int
                ,top_n_tokens :: Int
                }
                deriving (Show, Data, Typeable)

sample = BPE{dict_file = def
            ,make_tokens = 0
            ,top_n_tokens = 0
            }

main = do
  args <- cmdArgs sample
  print args
  
  inputList <- readFile (dict_file args)
  let state@(encodedText, decodeTable) = makeNTokens (textToSimpleTokens inputList) (make_tokens args)
  print $ humanReadebleRankings (topNTokens state (top_n_tokens args)) decodeTable

