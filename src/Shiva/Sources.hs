module Shiva.Sources where

import Shiva.Extract (extractDN)

import Data.Map  (Map, fromList)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Text (Text)



data Source = Source
  { sourceTitle :: String
  , feedUrl :: String
  , contentExtractor :: Text -> Text }

titleCode :: Source -> String
titleCode = intercalate "-" . words . map toLower . sourceTitle

codeMap :: Map String Source
codeMap = fromList $ zip (titleCode <$> sources) sources


---

latestNews, economy, stockholm :: Source
latestNews = Source "Latest News" "http://www.dn.se/rss/senaste-nytt/" extractDN
economy    = Source "Economy"     "http://www.dn.se/ekonomi/rss/"      extractDN
stockholm  = Source "Stockholm"   "http://www.dn.se/sthlm/rss/"        extractDN


sources :: [Source]
sources = [latestNews, economy, stockholm]
