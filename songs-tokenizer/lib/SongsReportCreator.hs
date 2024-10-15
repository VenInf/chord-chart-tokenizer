module SongsReportCreator where


import           Colonnade
import           Data.List              (group, sort, zipWith4)
import           Data.List.Extra        (sortOn)
import qualified Data.Maybe             as MB
import           Songs
import           Text.PrettyPrint.Boxes



data Ranks = Ranks { place       :: Int
                   , amount      :: Int
                   , probability :: Float
                   , token       :: String
                   } deriving (Show)


makeRanks :: Songs -> [Ranks]
makeRanks sngs = zipWith4 Ranks [1..] amounts probabilities tokens
    where
        tokenViews = concat $ MB.mapMaybe tokenView (songs sngs)
        tokenAmounts = sortOn (negate . snd) $ map (\xs -> (head xs, length xs)) . group . sort $ tokenViews

        amounts = map snd tokenAmounts
        totalAmount = toEnum $ sum amounts
        probabilities = map ((/ totalAmount) . toEnum) amounts
        tokens = map fst tokenAmounts


colRanks :: Colonnade Headed Ranks String
colRanks = mconcat
        [ headed "Place" (show . place)
        , headed "Amount" (show . amount)
        , headed "%" (show . (* 100) . probability)
        , headed "Token" token
        ]


makeStats :: Songs -> [Ranks] -> Box
makeStats songs ranks = stats
    where
        precentBool = zip (map probability ranks) (map ((> 1) . length . words . token) ranks)
        meaningfulPercent = (* 100) $ sum $ map fst $ filter snd precentBool

        shannonInformation = (-1) * sum [p * logBase 2 p | p <- map probability ranks]

        stats = vcat top [ hcat top $ map text ["% of covered by meaningful blocks ", show meaningfulPercent]
                         , hcat top $ map text ["information/entropy by Shannon ", show shannonInformation]
                         ]

createReport :: Songs -> String
createReport sngs = report
    where
        ranks = ascii colRanks $ makeRanks sngs
        stats = render $ makeStats sngs (makeRanks sngs)

        report = unlines [ "Information about found tokens:"
                         , stats
                         , ranks ]
