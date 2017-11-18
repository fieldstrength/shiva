{-# LANGUAGE Arrows                #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

module Shiva.Table.ArticleContent where

import Shiva.Config
import Shiva.Database             (runDbAction)
import Shiva.Translation

import Control.Arrow
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text                  (Text)
import Data.Time
import GHC.Generics
import Opaleye

data ArticleContent' a b c d = ArticleContent
    { urlFrag     :: a
    , content     :: b
    , createdAt   :: c
    , updatedAt   :: d
    } deriving (Show, Eq, Ord, Generic)

$(makeAdaptorAndInstance "pArticleContent" ''ArticleContent')

type ArticleContent = ArticleContent' Text TransArticle UTCTime UTCTime

type ArticleContentR = ArticleContent' (Column PGText)
                                       (Column PGJsonb)
                                       (Column PGTimestamptz)
                                       (Column PGTimestamptz)

type ArticleContentW = ArticleContent' (Column PGText)
                                       (Column PGJsonb)
                                       (Maybe (Column PGTimestamptz))
                                       (Maybe (Column PGTimestamptz))

toW :: TransArticle -> ArticleContentW
toW x = ArticleContent
    { urlFrag   = pgStrictText (urlFragment x)
    , content   = pgValueJSONB x
    , createdAt = Nothing
    , updatedAt = Nothing
    }

table :: Table ArticleContentW ArticleContentR
table = Table "article_content" $ pArticleContent ArticleContent
    { urlFrag    = required "url_fragment"
    , content    = required "content"
    , createdAt  = optional "created_at"
    , updatedAt  = optional "updated_at"
    }

queryAll :: Query ArticleContentR
queryAll = queryTable table

insert :: [TransArticle] -> ShivaM Int
insert xs = fmap fromIntegral . runDbAction $ \conn ->
    runInsertMany conn table (toW <$> xs)

get :: Text -> ShivaM [ArticleContent]
get frag = runDbAction $ \conn ->
    runQuery conn $ proc () -> do
        x <- queryAll -< ()
        restrict -< urlFrag x .=== pgStrictText frag
        returnA -< x
