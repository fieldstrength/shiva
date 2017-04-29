{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Shiva.Storage where

import Shiva.Config
import Shiva.Feeds
import qualified Shiva.Table.ArticleContent as Content
import qualified Shiva.Table.ArticleMetadata as Meta

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

-- make record
type ContentData = (Text,Text,Text)

fromTriple :: ContentData -> Content.ArticleContentIn
fromTriple (urlFrag, svBody, enBody) = Content.ArticleContent
    { Content.urlFrag   = urlFrag
    , Content.svBody    = svBody
    , Content.enBody    = enBody
    , Content.createdAt = ()
    , Content.updatedAt = ()
    }

toTriple :: Content.ArticleContent -> ContentData
toTriple Content.ArticleContent {..} = (urlFrag, svBody, enBody)

readContent :: Text -> ShivaM (Maybe ContentData)
readContent = fmap (headMay . map toTriple) . Content.get

writeContent :: Text -> Text -> Text -> ShivaM ()
writeContent urlFrag svBody enBody = void $
    Content.insert [Content.ArticleContent {..}]
    where
        createdAt = ()
        updatedAt = ()

{-

----- Article Content

type ContentData = (Text,Text,Text)

readContentData :: Text -> ShivaM (Maybe (Text,Text))
readContentData urlfrag = runDbAction $ \conn -> do
    l <- query conn "SELECT sv_body, en_body FROM article_content WHERE url_fragment = ?" (Only urlfrag)
    return $ headMay l

writeContentData :: ContentData -> ShivaM ()
writeContentData c = runDbAction $ \conn -> void $
    execute conn "INSERT INTO article_content (url_fragment, sv_body, en_body) VALUES (?,?,?)" c
-}
