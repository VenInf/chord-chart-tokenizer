{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main (main) where

import System.Console.CmdArgs
import Tokenizer

data BPE = BPE {dict_file :: FilePath
                ,make_tokens :: Int
                }
                deriving (Show, Data, Typeable)

sample = BPE{dict_file = def
            ,make_tokens = 0
            }

main = do
  args <- cmdArgs sample
  print args
  
  inputList <- readFile (dict_file args)
  let (encodedText, decodeTable) = makeNTokens (textToSimpleTokens inputList) (make_tokens args)
  print $ head encodedText