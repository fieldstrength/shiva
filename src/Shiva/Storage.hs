{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Shiva.Storage where

import Shiva.Config
import Shiva.Feeds
import qualified Shiva.Table.ArticleContent as Content
import qualified Shiva.Table.ArticleMetadata as Meta
import Shiva.Translation

import Control.Monad              (void)
import Data.Text                  (Text) -- , pack, unpack)
import Safe                       (headMay)


fromItem :: FeedItem -> Meta.ArticleMetadataIn
fromItem FeedItem {..} = Meta.ArticleMetadata
    { Meta.itemTime = itemTime
    , Meta.sourceName = sourceName
    , Meta.svTitle = svTitle
    , Meta.enTitle = enTitle
    , Meta.urlFrag = urlFrag
    , Meta.urlFull = urlFull
    , Meta.createdAt = ()
    , Meta.updatedAt = ()
    }

toItem :: Meta.ArticleMetadata -> FeedItem
toItem Meta.ArticleMetadata {..} = FeedItem {..}

writeMetadata :: [FeedItem] -> ShivaM ()
writeMetadata = void . Meta.insert . map fromItem

readMetadata :: Text -> ShivaM (Maybe FeedItem)
readMetadata = fmap (headMay . map toItem) . Meta.get

readMetaPairs :: Source -> ShivaM [(Text, Text)]
readMetaPairs = Meta.getPairs . sourceTitle


readContent :: Text -> ShivaM (Maybe TransArticle)
readContent = fmap (headMay . map Content.content) . Content.get

writeContent :: [TransArticle] -> ShivaM ()
writeContent = void . Content.insert
