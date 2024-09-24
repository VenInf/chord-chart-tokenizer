{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Main (main) where

import           Colonnade              (Colonnade, Headed, ascii, headed)
import qualified Control.Monad
import           Data.Maybe             (fromJust, fromMaybe)
import           StateSaver             (saveTo)
import           System.Console.CmdArgs (Data, Default (def), Typeable, cmdArgs,
                                         help, typFile, (&=))
import           System.FilePath
import           Tokenizer              (TokenizerState (decodeTable),
                                         humanReadebleRankings,
                                         textToTokenizerStateWithDict,
                                         topTokens)
import           TokenizerTriple
import           TokenToBlock           (tokenToBlock)
import           Data.Bifunctor         (first)
import Data.List (zipWith4)

data BPE = BPE { text_path       :: FilePath
               , dict_path       :: FilePath
               , tokens_per_step :: Int
               , steps           :: Int

               , save_state_path :: FilePath
               , report_path     :: FilePath
               }
               deriving (Show, Read, Data, Typeable)

defaultArgs :: BPE
defaultArgs = BPE
              { text_path = def &= typFile &= help "Get data for tokenization from file as a text"
              , dict_path = def &= typFile &= help "Token's dictionary file, each token on a separate line, higher have a priority"

              , tokens_per_step = 0 &= help "How many tokens should it create before doing a report"
              , steps = 0 &= help "How many steps of creating reports should it do in total"

              , save_state_path = def &= typFile &= help "Saves the internal state by the path after every report"
              , report_path = def &= help "Where to save the reports"
              }


colBPE :: Colonnade Headed BPE String
colBPE = mconcat
        [ headed "Text" text_path
        , headed "Dictionary" dict_path
        , headed "Tokens per step" (show . tokens_per_step)
        , headed "Save states to" save_state_path
        , headed "Reports" report_path
        ]

data Ranks = Ranks { place  :: Int
                   , token  :: String
                   , block  :: String
                   , amount :: Int
                   } deriving (Show)

pairToRanks :: [(String, Int)] -> [Ranks]
pairToRanks tokenAmounts = zipWith4 Ranks [1..] tokens blocks amounts
    where
        amounts = map snd tokenAmounts
        tokens = map fst tokenAmounts
        blocks = map tokenToBlock tokens

colRanks :: Colonnade Headed Ranks String
colRanks = mconcat
        [ headed "Place" (show . place)
        , headed "Amount" (show . amount)
        , headed "Token" token
        , headed "Block" block
        ]

loadTokenizerState :: FilePath -> IO TokenizerState
loadTokenizerState filePath = do
    stateString <- readFile filePath
    return (read stateString :: TokenizerState)

makeFileName :: FilePath -> Int -> Int -> FilePath
makeFileName dir step amoutOfTokens = dir </> "step-" <> show step <> "-make-tokens-" <> show amoutOfTokens <> ".txt"

makeReport :: TokenizerState -> FilePath -> IO()
makeReport state reportFileName = writeFile reportFileName report
    where
        ranks = ascii colRanks $ pairToRanks $ humanReadebleRankings (topTokens state) state
        report = unlines [ "Tokens by popularity:"
                         , ranks ]

makeFinalReport :: TokenizerState -> FilePath -> IO()
makeFinalReport state reportFileName = writeFile reportFileName report
    where
        tokens = reverse $ map snd $ decodeTable state
        hrRankingsTable = humanReadebleRankings (topTokens state) state
        tokenAmounts = map (fromMaybe 0 . (`lookup` hrRankingsTable)) tokens
        ranks = ascii colRanks $ pairToRanks $ zip tokens tokenAmounts
        report = unlines [ "Tokens by how early they were added :"
                         , ranks ]

makeReports :: TokenizerState -> BPE -> IO()
makeReports initialState bpe = go initialState 0
    where
        reportDir = report_path bpe
        stateDir = save_state_path bpe

        totalSteps = steps bpe
        tokensPerStep = tokens_per_step bpe

        go :: TokenizerState -> Int -> IO()
        go state repNumber = do
            let reportFileName = makeFileName reportDir repNumber tokensPerStep
                stateFileName = makeFileName stateDir repNumber tokensPerStep

            -- putStrLn $ "Making " ++ reportFileName
            -- makeReport state reportFileName
            -- putStrLn $ "Making " ++ stateFileName
            -- saveTo state stateFileName
            Control.Monad.when (repNumber < totalSteps) $ go (makeNTokens state tokensPerStep) (repNumber + 1)
            Control.Monad.when (repNumber == totalSteps) $ do
                makeFinalReport state (reportDir </> "final-standings.txt")
                let tokens = map snd $ decodeTable state
                writeFile (reportDir </> "final-tokens.txt") (unlines tokens)



handlePathArgs :: BPE -> IO()
handlePathArgs args = do
    if text_path args == def || tokens_per_step args == def || steps args == def
       || tokens_per_step args == def || report_path args == def
    then error "No critical arguments"
    else if dict_path args == def
        then do txt <- readFile (text_path args)
                let tokenizerState = textToTokenizerStateWithDict (lines txt) []
                makeReports tokenizerState args
        else do txt <- readFile (text_path args)
                dict <- readFile (dict_path args)
                let tokenizerState = textToTokenizerStateWithDict (lines txt) (lines dict)
                makeReports tokenizerState args

main :: IO()
main = do
  args <- cmdArgs defaultArgs
  putStrLn "Using following arguments:"
  putStrLn (ascii colBPE [args])

  handlePathArgs args
  putStrLn "Done"
