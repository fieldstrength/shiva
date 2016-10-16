{-# LANGUAGE RecordWildCards #-}

module Shiva.Feeds (
  FeedData (..),
  FeedItem (..),
  showTime,
  parseTime,
  parseTime',
  loadFeedPrelim,
) where

import Shiva.Config
import Shiva.Utils                (nothingMsg, safeHead, safeLast, separate)
import Shiva.Get                  (httpGet)

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

-- | RSS date format used by DN News feed.
--   for reference:
--   http://hackage.haskell.org/package/time-1.6.0.1/docs/Data-Time-Format.html#v:formatTime
timeFormat1 :: String
timeFormat1 = "%a, %e %b %Y %H:%M:%S %z"

timeFormat2 :: String
timeFormat2 = "%a, %e %b %Y %H:%M:%S %Z"

-- | If any other time formats incountered, add them to this list.
timeFormats :: [String]
timeFormats = [timeFormat1,timeFormat2]


parseTimeFormat :: String -> String -> Maybe UTCTime
parseTimeFormat f = fmap fst . safeHead . readSTime True defaultTimeLocale f

parseTime :: String -> Maybe UTCTime
parseTime s = safeHead . catMaybes $ parseTimeFormat <$> timeFormats <*> [s]

parseTime' :: String -> UTCTime
parseTime' = fromJust . parseTime

showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale timeFormat2


---- Retrieving RSS ----

type Err = Either String


interpRSS :: XmlSource s => s -> Err RSS
interpRSS = nothingMsg "Problem interpreting feed XML." . (elementToRSS <=< parseXMLDoc)

loadRSS :: String -> IOX RSS
loadRSS url = do
  mrss <- interpRSS <$> httpGet url
  case mrss of
    Left er -> throwError er
    Right r -> return r


----


data FeedItem = FeedItem
  { itemTime :: UTCTime
  , svTitle :: String
  , enTitle :: String
  , urlFrag :: String
  , urlFull :: String } deriving (Show,Eq,Ord)

data FeedData = FeedData
   { feedItems :: [FeedItem]
   , invalidItems :: [String] } deriving Show


extractRSSItems :: RSS -> [RSSItem]
extractRSSItems = rssItems . rssChannel

mkPrelim :: RSSItem -> Err FeedItem
mkPrelim RSSItem {..} = FeedItem
  <$> nothingMsg "Date invalid or missing" (rssItemPubDate >>= parseTime)
  <*> nothingMsg "Title invalid or missing" rssItemTitle
  <*> pure ""
  <*> linkMsg (rssItemLink >>= extractURLFrag)
  <*> linkMsg rssItemLink
    where linkMsg = nothingMsg "Link invalid or missing"
          extractURLFrag = safeLast . separate '/'


loadItems :: String -> IOX [Err FeedItem]
loadItems url = map mkPrelim . extractRSSItems <$> loadRSS url

loadPrelimItems :: String -> IOX FeedData
loadPrelimItems url = do
  mxs <- loadItems url
  return $ FeedData (rights mxs) (lefts mxs)

loadFeedPrelim :: Source -> IOX FeedData
loadFeedPrelim = loadPrelimItems . feedUrl
