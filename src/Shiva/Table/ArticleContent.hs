{-# LANGUAGE Arrows                #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

module Shiva.Table.ArticleContent where

import Shiva.Config
import Shiva.Database             (runDbAction)

import Control.Arrow
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text                  (Text)
import Data.Time
import GHC.Generics
import Opaleye

data ArticleContent' a b c d e
    = ArticleContent
    { urlFrag     :: a
    , svBody      :: b
    , enBody      :: c
    , createdAt   :: d
    , updatedAt :: e
    } deriving (Show, Eq, Ord, Generic)

$(makeAdaptorAndInstance "pArticleContent" ''ArticleContent')

type ArticleContent = ArticleContent' Text Text Text UTCTime UTCTime

type ArticleContentIn = ArticleContent' Text Text Text () ()

type ArticleContentR = ArticleContent' (Column PGText)
                                       (Column PGText)
                                       (Column PGText)
                                       (Column PGTimestamptz)
                                       (Column PGTimestamptz)

type ArticleContentW = ArticleContent' (Column PGText)
                                       (Column PGText)
                                       (Column PGText)
                                       (Maybe (Column PGTimestamptz))
                                       (Maybe (Column PGTimestamptz))

toW :: ArticleContentIn -> ArticleContentW
toW x = ArticleContent
    { urlFrag   = pgStrictText (urlFrag x)
    , svBody    = pgStrictText (svBody x)
    , enBody    = pgStrictText (enBody x)
    , createdAt = Nothing
    , updatedAt = Nothing
    }

table :: Table ArticleContentW ArticleContentR
table = Table "article_metadata" $ pArticleContent ArticleContent
    { urlFrag    = required "url_fragment"
    , svBody     = required "sv_body"
    , enBody     = required "en_body"
    , createdAt  = optional "created_at"
    , updatedAt  = optional "updated_at"
    }

queryAll :: Query ArticleContentR
queryAll = queryTable table

insert :: [ArticleContentIn] -> ShivaM Int
insert xs = fmap fromIntegral . runDbAction $ \conn ->
    runInsertMany conn table (toW <$> xs)

get :: Text -> ShivaM [ArticleContent]
get frag = runDbAction $ \conn ->
    runQuery conn $ proc () -> do
        x <- queryAll -< ()
        restrict -< urlFrag x .=== pgStrictText frag
        returnA -< x
