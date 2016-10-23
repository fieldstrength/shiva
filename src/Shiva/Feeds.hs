{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Shiva.Feeds (
  FeedData (..),
  FeedItem (..),
  showTime,
  parseTime,
  unsafeParseTime,
  loadFeedPrelim,
) where

import Shiva.Config
import Shiva.Utils                (nothingMsg, separate)
import Shiva.Get                  (httpGet)

import Safe                       (headMay, lastMay)
import Control.Monad              ((<=<))
import Data.Maybe                 (fromJust, catMaybes)
import Text.XML.Light.Lexer       (XmlSource)
import Text.XML.Light.Input       (parseXMLDoc)
import Text.RSS.Import            (elementToRSS)
import Text.RSS.Syntax            (RSS (..), RSSItem (..), RSSChannel (..))
import Control.Monad.Error.Class  (throwError)
import Data.Time.Format           (readSTime, formatTime)
import Data.Time                  (UTCTime, defaultTimeLocale)
import Data.Either                (rights, lefts)
import Data.Text                  (Text, pack, unpack)

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


interpRSS :: XmlSource s => s -> Err RSS
interpRSS = nothingMsg "Problem interpreting feed XML." . (elementToRSS <=< parseXMLDoc)

loadRSS :: Text -> IOX RSS
loadRSS url = do
  mrss <- interpRSS <$> httpGet url
  case mrss of
    Left er -> throwError er
    Right r -> return r


----


data FeedItem = FeedItem
  { itemTime :: UTCTime
  , sourceName :: Text
  , svTitle :: Text
  , enTitle :: Text
  , urlFrag :: Text
  , urlFull :: Text } deriving (Show,Eq,Ord)

data FeedData = FeedData
   { feedItems :: [FeedItem]
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


loadItems :: Text -> Text -> IOX [Err FeedItem]
loadItems name url = map (mkPrelim name) . extractRSSItems <$> loadRSS url

loadFeedPrelim :: Source -> IOX FeedData
loadFeedPrelim Source {..} = do
  mxs <- loadItems sourceTitle (pack feedUrl)
  return $ FeedData (rights mxs) (lefts mxs)
