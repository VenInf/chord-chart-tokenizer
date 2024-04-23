module Lib
    ( someFunc
    ) where
import Data.List (nub)


someFunc :: IO ()
someFunc = putStrLn "someFunc"

getInitialVocab :: String -> String
getInitialVocab = nub