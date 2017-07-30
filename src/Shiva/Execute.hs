{-# LANGUAGE RecordWildCards #-}

-- | Assembles functionality from several modules to perform main business logic.
module Shiva.Execute (
  loadFeedByTitleCode,
  generateResultFromName,
  catchErrorPage,
) where

-- initTransDataIO :: SubscriptionKey -> IO (Either TranslatorException TransData)
-- translateArrayIO :: TransData -> Language -> Language -> [Text] -> IO (Either TranslatorException ArrayResponse)
-- newtype ArrayResponse = ArrayResponse { getArrayResponse :: [TransItem] }
{-  data TransItem = TransItem
        { transText        :: Text
        , originalBreaks   :: [Int]
        , translatedBreaks :: [Int]  -}

import Shiva.Config
import Shiva.Storage
import Shiva.Feeds
import Shiva.Get           (httpGet)
import Shiva.HTML
import Shiva.Translation

import Control.Monad.Catch (catchAll, throwM)
import Control.Monad.State (lift)
import Data.Bifunctor      (first, second)
import Data.List           (sortBy)
import Data.Map            (Map, fromList, lookup)
import Data.Text           (Text, empty)
import Lucid
import Prelude             hiding (lookup)

readMetadataMap :: Source -> ShivaM (Map Text Text)
readMetadataMap = fmap fromList . readMetaPairs

subStep :: Map Text Text -> FeedItem -> ([FeedItem],[FeedItem]) -> ([FeedItem],[FeedItem])
subStep m i = case lookup (svTitle i) m of
  Just eng -> first  (d:) where d = i {enTitle = eng}
  Nothing  -> second (i:)

subMetadata :: Map Text Text -> [FeedItem] -> ([FeedItem],[FeedItem])
subMetadata m = foldr (subStep m) ([],[])


-- | Take list of feed items without english titles, fill them in by translating the swedish titles.
translateTitles :: [FeedItem] -> ShivaM [FeedItem]
translateTitles ds = do
  ps <- translateSet $ svTitle <$> ds
  pure $ zipWith (\d p -> d {enTitle = english p }) ds ps

loadFeedData :: Source -> ShivaM FeedData
loadFeedData src = do
  fd <- ShivaM <$> lift $ loadFeedPrelim src
  m  <- readMetadataMap src
  let (xs,ys) = subMetadata m (feedItems fd)
  zs <- translateTitles ys
  writeMetadata zs
  pure $ fd { feedItems = sortBy (flip compare) (xs ++ zs) }

-- | Load the RSS feed page for a given 'Source' by specifying it's 'titleCode'.
loadFeedByTitleCode :: Text -> ShivaM FeedData
loadFeedByTitleCode code = do
  msrc <- codeLookup code
  case msrc of
    Just src -> loadFeedData src
    Nothing  -> throwM $ UnknownFeed code


-- | If an error is encountered in the ShivaM monad, report the error with a webpage.
catchErrorPage :: ShivaM (Html ()) -> ShivaM (Html ())
catchErrorPage = flip catchAll (pure . errorPage . show)


-- | Given swedish and english text separated by |, return the corresponding ShivaResult
genResult :: Text -> Text -> ShivaResult
genResult sv en =
  let svs = barDiv sv
      ens = barDiv en
      ps  = zipWithDefault SvenskaPair empty svs ens
  in ShivaResult (length svs == length ens) ps


-- | Take an URL fragment (functioning as an identifier) and text, then translate the text,
--   save the result to the database, and return it.
translateSaveBodyText :: Text -> Text -> ShivaM ShivaResult
translateSaveBodyText ufrag sv' = do
    TransResult result sv en <- translateArticleText sv'
    writeContent ufrag sv en
    pure result

retrieveAndExtract :: FeedItem -> ShivaM TransArticle
retrieveAndExtract FeedItem {..} = do
  txt <- ShivaM <$> lift $ httpGet urlFull
  msrc <- srcLookup sourceName
  case msrc of
    Nothing -> throwM $ UnknownSourceName sourceName
    Just Source {..} -> do
      let contentTxt = contentExtractor txt
          img        = imageExtractor   txt
      r <- translateSaveBodyText urlFrag contentTxt
      pure $ TransArticle svTitle urlFull img r


generateContentResult :: FeedItem -> ShivaM TransArticle
generateContentResult fi@FeedItem {..} = do
  mx <- readContent urlFrag
  case mx of
    Nothing   -> retrieveAndExtract fi
    Just (_, svBody, enBody) -> pure $
        TransArticle svTitle urlFull Nothing $
            genResult svBody enBody


-- | Used to generate a web page for an article, identified by a part of a URL. This relies on the
--   article metadata already being in the database, due to its appearing in an RSS listing page.
generateResultFromName :: Text -> ShivaM TransArticle
generateResultFromName urlfrag = do
  md <- readMetadata urlfrag
  case md of
    Just d  -> generateContentResult d
    Nothing -> throwM $ MissingArticle urlfrag
