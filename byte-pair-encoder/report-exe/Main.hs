{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import           StateSaver             (saveTo)
import           System.Console.CmdArgs (Data, Default (def), Typeable, cmdArgs,
                                         help, typFile, (&=))
import           System.Exit
import           Tokenizer



main :: IO()
main = do

  putStrLn "Using following arguments"
  putStrLn "Done"
