{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main (main) where

import Lib
import System.Console.CmdArgs

data BPE = BPE {dict_file :: FilePath
                ,vocab_size :: Int
                }
                deriving (Show, Data, Typeable)

sample = BPE{dict_file = def
            ,vocab_size = 0
            }

main = do
  args <- cmdArgs sample
  print args
  file <- readFile (dict_file args)
  putStr file