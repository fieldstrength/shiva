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
import Control.Monad.Reader
import Control.Monad (void)


myConnectInfo :: ShivaM ConnectInfo
myConnectInfo = do
  Config {..} <- ask
  return $ defaultConnectInfo { connectUser = dbUser, connectDatabase = dbName }

initConnection :: ShivaM Connection
initConnection = liftIO . connect =<< myConnectInfo

tuplize :: Source -> FeedItem -> (String,String,String,String,String,String)
tuplize src p = (showTime (itemTime p), sourceTitle src, svTitle p, enTitle p, urlFrag p, urlFull p)

deTuplize :: (String,String,String,String,String,String) -> FeedItem
deTuplize (_,t,s,e,u,v) = FeedItem (parseTime' t) s e u v


writeAritcleMetadata :: Source -> [FeedItem] -> ShivaM ()
writeAritcleMetadata cat xs = do
  conn <- initConnection
  void . liftIO $ executeMany conn
    "INSERT INTO articleMetaData (date, category, svTitle, enTitle, urlFrag, urlFull) VALUES (?,?,?,?,?,?)" $
    tuplize cat <$> xs

readArticleMetadata :: String -> ShivaM (Maybe FeedItem)
readArticleMetadata name = do
  conn <- initConnection
  l <- liftIO $ query conn "SELECT * FROM articleMetaData WHERE urlFrag = ?" (Only name)
  return . safeHead $ deTuplize <$> l


readPairs :: Source -> ShivaM [(String,String)]
readPairs src = do
  conn <- initConnection
  liftIO $ query conn "SELECT svTitle, enTitle FROM articleMetaData WHERE category = ?" (Only $ sourceTitle src)


----- Article Content

type ContentData = (String,String,String)

readContentData :: String -> ShivaM (Maybe (String,String))
readContentData urlfrag = do
  conn <- initConnection
  l <- liftIO $ query conn "SELECT svBody, enBody FROM articleContent WHERE urlFrag = ?" (Only urlfrag)
  return $ safeHead l

writeContentData :: ContentData -> ShivaM ()
writeContentData c = do
  conn <- initConnection
  void . liftIO $ execute conn "INSERT INTO articleContent (urlFrag, svBody, enBody) VALUES (?,?,?)" c


readCharCount :: ShivaM Int
readCharCount = do
  conn <- initConnection
  [Only n] <- liftIO $ query_ conn "SELECT * FROM charCount"
  return n

writeCharCount :: Int -> ShivaM ()
writeCharCount n = do
  conn <- initConnection
  void . liftIO $ execute conn "UPDATE charCount SET numChars = ?" (Only n)
