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


createReport :: Songs -> String
createReport sngs = report
    where
        ranks = ascii colRanks $ makeRanks sngs
        report = unlines [ "Information about found tokens:"
                         , ranks ]
