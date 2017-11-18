{-# LANGUAGE Arrows                #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

module Shiva.Table.ArticleMetadata where

import Shiva.Config
import Shiva.Database             (runDbAction)

import Control.Arrow
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text                  (Text)
import Data.Time
import GHC.Generics
import Opaleye

data ArticleMetadata' a b c d e f g h = ArticleMetadata
    { itemTime   :: a
    , sourceName :: b
    , svTitle    :: c
    , enTitle    :: d
    , urlFrag    :: e
    , urlFull    :: f
    , createdAt  :: g
    , updatedAt  :: h
    } deriving (Show, Eq, Ord, Generic)

$(makeAdaptorAndInstance "pArticleMetadata" ''ArticleMetadata')

type ArticleMetadata = ArticleMetadata' UTCTime Text Text Text Text Text UTCTime UTCTime

type ArticleMetadataIn = ArticleMetadata' UTCTime Text Text Text Text Text () ()

type ArticleMetadataR = ArticleMetadata' (Column PGTimestamptz)
                                         (Column PGText)
                                         (Column PGText)
                                         (Column PGText)
                                         (Column PGText)
                                         (Column PGText)
                                         (Column PGTimestamptz)
                                         (Column PGTimestamptz)

type ArticleMetadataW = ArticleMetadata' (Column PGTimestamptz)
                                         (Column PGText)
                                         (Column PGText)
                                         (Column PGText)
                                         (Column PGText)
                                         (Column PGText)
                                         (Maybe (Column PGTimestamptz))
                                         (Maybe (Column PGTimestamptz))

toW :: ArticleMetadataIn -> ArticleMetadataW
toW x = ArticleMetadata
    { itemTime   = pgUTCTime (itemTime x)
    , sourceName = pgStrictText (sourceName x)
    , svTitle    = pgStrictText (svTitle x)
    , enTitle    = pgStrictText (enTitle x)
    , urlFrag    = pgStrictText (urlFrag x)
    , urlFull    = pgStrictText (urlFull x)
    , createdAt  = Nothing
    , updatedAt  = Nothing
    }

table :: Table ArticleMetadataW ArticleMetadataR
table = Table "article_metadata" $ pArticleMetadata ArticleMetadata
    { itemTime   = required "published_on"
    , sourceName = required "source_name"
    , svTitle    = required "sv_title"
    , enTitle    = required "en_title"
    , urlFrag    = required "url_fragment"
    , urlFull    = required "url"
    , createdAt  = optional "created_at"
    , updatedAt  = optional "updated_at"
    }

queryAll :: Query ArticleMetadataR
queryAll = queryTable table

insert :: [ArticleMetadataIn] -> ShivaM Int
insert xs = fmap fromIntegral . runDbAction $ \conn ->
    runInsertMany conn table (toW <$> xs)

get :: Text -> ShivaM [ArticleMetadata]
get frag = runDbAction $ \conn ->
    runQuery conn $ proc () -> do
        x <- queryAll -< ()
        restrict -< urlFrag x .=== pgStrictText frag
        returnA -< x

getPairs :: Text -> ShivaM [(Text, Text)]
getPairs src = runDbAction $ \conn ->
    runQuery conn $ proc () -> do
        x <- queryAll -< ()
        restrict -< sourceName x .=== pgStrictText src
        returnA -< (svTitle x, enTitle x)
