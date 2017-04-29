{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Shiva.Feeds (
  FeedData (..),
  FeedItem (..),
  showTime,
  parseTime,
  unsafeParseTime,
  loadFeedPrelim,
) where

import Shiva.Config
import Shiva.Get            (httpGet)
import Shiva.Utils          (nothingMsg, separate)

import Control.Monad        ((<=<))
import Control.Monad.Catch  (throwM)
import Data.Either          (lefts, rights)
import Data.Maybe           (catMaybes, fromJust)
import Data.Text            (Text, pack)
import Data.Time            (UTCTime, defaultTimeLocale)
import Data.Time.Format     (formatTime, readSTime)
import Safe                 (headMay, lastMay)
import Text.RSS.Import      (elementToRSS)
import Text.RSS.Syntax      (RSS (..), RSSChannel (..), RSSItem (..))
import Text.XML.Light.Input (parseXMLDoc)
import Text.XML.Light.Lexer (XmlSource)


-- | RSS date format used by DN News feed.
--   for reference:
--   http://hackage.haskell.org/package/time-1.6.0.1/docs/Data-Time-Format.html#v:formatTime
timeFormat1 :: String
timeFormat1 = "%a, %e %b %Y %H:%M:%S %z"

timeFormat2 :: String
timeFormat2 = "%a, %e %b %Y %H:%M:%S %Z"

-- | If any other time formats are encountered, add them to this list.
timeFormats :: [String]
timeFormats = [timeFormat1,timeFormat2]


parseTimeFormat :: String -> String -> Maybe UTCTime
parseTimeFormat f = fmap fst . headMay . readSTime True defaultTimeLocale f

parseTime :: String -> Maybe UTCTime
parseTime s = headMay . catMaybes $ parseTimeFormat <$> timeFormats <*> [s]

unsafeParseTime :: String -> UTCTime
unsafeParseTime = fromJust . parseTime

showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale timeFormat2


---- Retrieving RSS ----

type Err = Either String


interpRSS :: XmlSource s => s -> Maybe RSS
interpRSS = elementToRSS <=< parseXMLDoc

loadRSS :: Text -> IO RSS
loadRSS url = do
  mRss <- interpRSS <$> httpGet url
  case mRss of
    Nothing  -> throwM $ PraseRSSFeedException "Couldn't parse RSS feed XML"
    Just rss -> pure rss


----

data FeedItem = FeedItem
  { itemTime   :: UTCTime
  , sourceName :: Text
  , svTitle    :: Text
  , enTitle    :: Text
  , urlFrag    :: Text
  , urlFull    :: Text } deriving (Show, Eq, Ord)

data FeedData = FeedData
   { feedItems    :: [FeedItem]
   , invalidItems :: [String] } deriving Show


extractRSSItems :: RSS -> [RSSItem]
extractRSSItems = rssItems . rssChannel

mkPrelim :: Text -> RSSItem -> Err FeedItem
mkPrelim srcName RSSItem {..} = FeedItem
  <$> nothingMsg "Date invalid or missing" (rssItemPubDate >>= parseTime)
  <*> pure srcName
  <*> nothingMsg "Title invalid or missing" (pack <$> rssItemTitle)
  <*> pure ""
  <*> linkMsg (fmap pack $ rssItemLink >>= extractURLFrag)
  <*> linkMsg (pack <$> rssItemLink)
    where linkMsg = nothingMsg "Link invalid or missing"
          extractURLFrag = lastMay . separate '/'


loadItems :: Text -> Text -> IO [Err FeedItem]
loadItems name url = map (mkPrelim name) . extractRSSItems <$> loadRSS url

loadFeedPrelim :: Source -> IO FeedData
loadFeedPrelim Source {..} = do
  mxs <- loadItems sourceTitle (pack feedUrl)
  pure $ FeedData (rights mxs) (lefts mxs)
