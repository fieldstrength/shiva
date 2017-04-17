{-# LANGUAGE OverloadedStrings #-}

module Shiva.Database (
  readArticleMetadata,
  readPairs,
  writeAritcleMetadata,

  readContentData,
  writeContentData,

) where

import Shiva.Feeds
import Shiva.Config

import Data.Text (Text,pack,unpack)
import Safe (headMay)
import Database.PostgreSQL.Simple
import Control.Monad.IO.Class
import Control.Monad (void)



runDbAction :: (Connection -> IO a) -> ShivaM a
runDbAction fio = liftIO . fio =<< appConnection


tuplize :: FeedItem -> (Text,Text,Text,Text,Text,Text)
tuplize p = (pack (showTime $ itemTime p), sourceName p, svTitle p, enTitle p, urlFrag p, urlFull p)

deTuplize :: (Text,Text,Text,Text,Text,Text) -> FeedItem
deTuplize (t,src,s,e,u,v) = FeedItem (unsafeParseTime $ unpack t) src s e u v


writeAritcleMetadata :: [FeedItem] -> ShivaM ()
writeAritcleMetadata xs = runDbAction $ \conn -> void $
  executeMany conn
  "INSERT INTO article_metadata (date, category, sv_title, en_title, url_fragment, url) VALUES (?,?,?,?,?,?)" $
  tuplize <$> xs

readArticleMetadata :: Text -> ShivaM (Maybe FeedItem)
readArticleMetadata name = runDbAction $ \conn -> do
  l <- query conn "SELECT (date, category, sv_title, en_title, url_fragment, url) \
                  \FROM article_metadata WHERE url_fragment = ?" (Only name)
  return . headMay $ deTuplize <$> l

readPairs :: Source -> ShivaM [(Text,Text)]
readPairs src = runDbAction $ \conn ->
  query conn "SELECT sv_title, en_title FROM article_metadata WHERE category = ?" (Only $ sourceTitle src)


----- Article Content

type ContentData = (Text,Text,Text)

readContentData :: Text -> ShivaM (Maybe (Text,Text))
readContentData urlfrag = runDbAction $ \conn -> do
  l <- query conn "SELECT sv_body, en_body FROM article_content WHERE url_fragment = ?" (Only urlfrag)
  return $ headMay l

writeContentData :: ContentData -> ShivaM ()
writeContentData c = runDbAction $ \conn ->
  void $ execute conn "INSERT INTO article_content (url_fragment, sv_body, en_body) VALUES (?,?,?)" c
