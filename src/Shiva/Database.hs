{-# LANGUAGE OverloadedStrings,
             RecordWildCards    #-}

module Shiva.Database (
  readArticleMetadata,
  readPairs,
  writeAritcleMetadata,

  readContentData,
  writeContentData,

  readCharCount,
  writeCharCount,
) where

import Shiva.Feeds
import Shiva.Config
import Shiva.Utils (safeHead)

import Database.PostgreSQL.Simple
import Control.Monad.IO.Class
import Control.Monad (void)



runDbAction :: (Connection -> IO a) -> ShivaM a
runDbAction fio = liftIO . fio =<< appConnection


tuplize :: FeedItem -> (String,String,String,String,String,String)
tuplize p = (showTime (itemTime p), sourceName p, svTitle p, enTitle p, urlFrag p, urlFull p)

deTuplize :: (String,String,String,String,String,String) -> FeedItem
deTuplize (t,src,s,e,u,v) = FeedItem (parseTime' t) src s e u v


writeAritcleMetadata :: [FeedItem] -> ShivaM ()
writeAritcleMetadata xs = runDbAction $ \conn -> void $
  executeMany conn
  "INSERT INTO articleMetaData (date, category, svTitle, enTitle, urlFrag, urlFull) VALUES (?,?,?,?,?,?)" $
  tuplize <$> xs

readArticleMetadata :: String -> ShivaM (Maybe FeedItem)
readArticleMetadata name = runDbAction $ \conn -> do
  l <- query conn "SELECT * FROM articleMetaData WHERE urlFrag = ?" (Only name)
  return . safeHead $ deTuplize <$> l

readPairs :: Source -> ShivaM [(String,String)]
readPairs src = runDbAction $ \conn ->
  query conn "SELECT svTitle, enTitle FROM articleMetaData WHERE category = ?" (Only $ sourceTitle src)


----- Article Content

type ContentData = (String,String,String)

readContentData :: String -> ShivaM (Maybe (String,String))
readContentData urlfrag = runDbAction $ \conn -> do
  l <- query conn "SELECT svBody, enBody FROM articleContent WHERE urlFrag = ?" (Only urlfrag)
  return $ safeHead l

writeContentData :: ContentData -> ShivaM ()
writeContentData c = runDbAction $ \conn ->
  void $ execute conn "INSERT INTO articleContent (urlFrag, svBody, enBody) VALUES (?,?,?)" c


readCharCount :: ShivaM Int
readCharCount = runDbAction $ \conn -> do
  [Only n] <- query_ conn "SELECT * FROM charCount"
  return n

writeCharCount :: Int -> ShivaM ()
writeCharCount n = runDbAction $ \conn ->
  void $ execute conn "UPDATE charCount SET numChars = ?" (Only n)
