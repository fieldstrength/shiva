{-# LANGUAGE RecordWildCards #-}

-- | Assembles functionality from several modules to perform main business logic.
module Shiva.Execute (
  loadFeedByTitleCode,
  generateResultFromName,
  catchErrorPage,
) where


import Shiva.Feeds
import Shiva.Config
import Shiva.Database
import Shiva.Translation
import Shiva.HTML
import Shiva.Get                 (httpGet)

import Data.Bifunctor            (first, second)
import Prelude hiding            (lookup)
import Data.List                 (sortBy)
import Data.Map                  (Map,lookup,fromList)
import Lucid
import Control.Monad.Error.Class (throwError, catchError)
import Control.Monad.State       (lift)
import Data.Text                 (Text,unpack,empty)


readMetadataMap :: Source -> ShivaM (Map Text Text)
readMetadataMap = fmap fromList . readPairs

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
  return $ zipWith (\d p -> d {enTitle = english p }) ds ps

loadFeedData :: Source -> ShivaM FeedData
loadFeedData src = do
  fd <- lift $ loadFeedPrelim src
  m  <- readMetadataMap src
  let (xs,ys) = subMetadata m (feedItems fd)
  zs <- translateTitles ys
  writeAritcleMetadata zs
  return $ fd { feedItems = sortBy (flip compare) (xs ++ zs) }

-- | Load the RSS feed page for a given 'Source' by specifying it's 'titleCode'.
loadFeedByTitleCode :: Text -> ShivaM FeedData
loadFeedByTitleCode code = do
  msrc <- codeLookup code
  case msrc of
    Just src -> loadFeedData src
    Nothing -> throwError "I don't recognize any feed with that title"


-- | If an error is encountered in the ShivaM monad, report the error with a webpage.
catchErrorPage :: ShivaM (Html ()) -> ShivaM (Html ())
catchErrorPage sh = catchError sh $ return . errorPage


-- | Given swedish and english text separated by |, return the corresponding ShivaResult
genResult :: Text -> Text -> ShivaResult
genResult sv en =
  let svs = barDiv sv
      ens = barDiv en
      ps  = zipWithDefault SvenskaPair empty svs ens
  in ShivaResult (length svs == length ens) ps

retrieveContent :: FeedItem -> ShivaM Text
retrieveContent FeedItem {..} = do
  txt <- lift $ httpGet urlFull
  msrc <- srcLookup sourceName
  case msrc of
    Just Source {..} -> return $ contentExtractor txt
    Nothing          -> throwError $ "retrieveContent: unknown sourceName '"
                          ++ unpack sourceName ++ "'."


-- | Take an URL fragment (functioning as an identifier) and text, then translate the text,
--   save the result to the database, and return it.
translateSaveBodyText :: Text -> Text -> ShivaM ShivaResult
translateSaveBodyText ufrag sv = do
  TransResult {..} <- translateArticleText sv
  writeContentData (ufrag,svTxt,enTxt)
  return theresult


generateContentResult :: FeedItem -> ShivaM TransArticle
generateContentResult fi@FeedItem {..} = do
  mx <- readContentData urlFrag
  case mx of
    Just (s,e) -> return $ TransArticle svTitle urlFull Nothing $ genResult s e
    Nothing    -> do
      art <- retrieveContent fi
      r <- translateSaveBodyText urlFrag art
      return $ TransArticle svTitle urlFull Nothing r


-- | Used to generate a web page for an article, identified by a part of a URL. This relies on the
--   article metadata already being in the database, due to its appearing in an RSS listing page.
generateResultFromName :: Text -> ShivaM TransArticle
generateResultFromName urlfrag = do
  md <- readArticleMetadata urlfrag
  case md of
    Just d  -> generateContentResult d
    Nothing -> throwError "Article by that name seems to be missing."
