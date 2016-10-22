{-# LANGUAGE OverloadedStrings #-}

module Shiva.Sources (
  extractDN,
  sources,
) where

import Shiva.Config  (Source (..))
import Shiva.Extract (extractDivText, extractImgUrl_noParams)

import Data.Text     (Text)


extractDN :: Text -> Text
extractDN = extractDivText ["article__body-grid-item article__lead", "article__body-content"]

extractDNImage :: Text -> Maybe Text
extractDNImage = extractImgUrl_noParams "image-box__img"

latestNews, economy, stockholm :: Source
latestNews = Source "Latest News" "http://www.dn.se/rss/senaste-nytt/" extractDN extractDNImage
economy    = Source "Economy"     "http://www.dn.se/ekonomi/rss/"      extractDN extractDNImage
stockholm  = Source "Stockholm"   "http://www.dn.se/sthlm/rss/"        extractDN extractDNImage

sources :: [Source]
sources = [latestNews, economy, stockholm]
