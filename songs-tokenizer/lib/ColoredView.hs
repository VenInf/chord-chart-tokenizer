{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module ColoredView where

import           Songs
import Data.List (nub)
import Data.Maybe (fromJust, fromMaybe)
import System.Console.ANSI
import Data.List.Extra (sort, dropPrefix)
import Data.List (inits)
import Data.List.Extra (unsnoc)
import Data.List (intersperse)
import Data.List

selectedColors :: [Color]
selectedColors = [Red, Green, Yellow, Blue, Magenta, Cyan]

printColoredSongs :: Songs -> IO ()
printColoredSongs sngs = mapM_ printColoredSong (songs sngs)

printColoredSong :: Song -> IO()
printColoredSong song = do
    putStrLn $ "Title: " <> title song
    putStrLn $ "Composer: " <> composedBy song

    let coloredBlocks = map (map (uncurry printStringColored)) (splitByNBars 5 blockColor)
        withSpaces = map (mapM_ (\a -> a >> putStr " ")) coloredBlocks
    mapM_ (\a -> do a >> putStr "\n") withSpaces
    putStr "\n"

    where
        uniqueTokens = sort $ nub $ fromJust $ tokenView song
        tokenColors = zip uniqueTokens selectedColors
        tokenBlock = zip (fromJust $ tokenView song) (fromJust $ blockView song)
        blockColor = map (\(tok, blk) -> (blk, fromMaybe Black $ lookup tok tokenColors)) tokenBlock

takeNBars :: Int -> [(String, Color)] -> [(String, Color)]
takeNBars n blkClr = case unsnoc lessThanNBars of
                        Just (_, []) -> fallbackCase
                        Nothing -> fallbackCase
                        Just (_, list) -> list
    where
        potentialLines = inits blkClr
        lessThanNBars = filter ((<= n) . length . concatMap (filter (== '|') . fst)) potentialLines
        fallbackCase = case blkClr of
                            (h:_) -> [h]
                            [] -> []

splitByNBars :: Int -> [(String, Color)] -> [[(String, Color)]]
splitByNBars _ [] = []
splitByNBars n blkClr = nBars : splitByNBars n (dropPrefix nBars blkClr)
    where
        nBars = takeNBars n blkClr


printStringColored :: String -> Color -> IO()
printStringColored str color = do
    setSGR [ SetConsoleIntensity BoldIntensity
           , SetColor Foreground Vivid color
           ]
    putStr str
    setSGR [ Reset ]
