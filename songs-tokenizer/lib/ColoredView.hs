{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ColoredView where

import           Data.List
import           Data.List.Extra     (dropPrefix, unsnoc)
import           Data.Maybe          (fromJust, fromMaybe)
import           Songs
import           System.Console.ANSI

selectedColors :: [Color]
selectedColors = [Red, Green, Yellow, Blue, Magenta, Cyan]

printColoredSongs :: Songs -> IO ()
printColoredSongs sngs = mapM_ printColoredSong (songs sngs)

printColoredSong :: Song -> IO()
printColoredSong song = do
    putStrLn $ "Title: " <> title song
    putStrLn $ "Composer: " <> composedBy song

    let coloredBlocks = map (map (uncurry printStringColored)) (splitByNBars 5 blockColor)
        withSpaces = map (mapM_ (\a -> a >> printStringColored " ^ " White)) coloredBlocks
    mapM_ (\a -> do a >> putStr "\n") withSpaces
    putStr "\n"

    where
        uniqueTokens = sort $ nub $ fromJust $ tokenView song
        tokenColors = zip uniqueTokens (cycle selectedColors)
        tokenBlock = zip (fromJust $ tokenView song) (fromJust $ blockView song)
        blockColor = map (\(tok, blk) -> (blk, fromMaybe Black $ lookup tok tokenColors)) tokenBlock

takeNBars :: Int -> [(String, Color)] -> [(String, Color)]
takeNBars n blkClr = case unsnoc lessThanNBars of
                        Just (_, [])   -> fallbackCase
                        Nothing        -> fallbackCase
                        Just (_, list) -> list
    where
        potentialLines = inits blkClr
        lessThanNBars = filter ((<= n) . length . concatMap (filter (== '|') . fst)) potentialLines
        fallbackCase = case blkClr of
                            (h:_) -> [h]
                            []    -> []

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
