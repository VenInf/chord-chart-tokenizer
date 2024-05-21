module StateSaver where

import Tokenizer

saveTo :: Show a => a -> FilePath -> IO ()
saveTo tState filePath = do
    writeFile filePath (show tState)

loadTokenizerState :: FilePath -> IO TokenizerState
loadTokenizerState filePath = do
    stateString <- readFile filePath
    return (read stateString :: TokenizerState)