{-# LANGUAGE OverloadedStrings #-}

module Shiva.Sources where

import Shiva.Config  (Source (..))
import Shiva.Extract (extractDivId)

import Data.Text     (Text)


extractDN :: Text -> Text
extractDN = extractDivId "article__body-content"

latestNews, economy, stockholm :: Source
latestNews = Source "Latest News" "http://www.dn.se/rss/senaste-nytt/" extractDN
economy    = Source "Economy"     "http://www.dn.se/ekonomi/rss/"      extractDN
stockholm  = Source "Stockholm"   "http://www.dn.se/sthlm/rss/"        extractDN

sources :: [Source]
sources = [latestNews, economy, stockholm]
