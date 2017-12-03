{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Shiva.HTML (

  -- * Pages
  feedPage,
  mainPage,
  articlePage,
  errorPage,

  -- * Helpers
  htmlFromEither

) where

import           Shiva.Config                (Source (..), titleCode)
import           Shiva.Feeds
import           Shiva.Sources               (sources)
import           Shiva.Translation

import           Control.Monad               (forM_, unless)
import           Data.Monoid                 ((<>))
import           Lucid.Base
import           Lucid.Html5
import           Microsoft.Translator
import           Shiva.Table.ArticleMetadata (ArticleMetadata)
import qualified Shiva.Table.ArticleMetadata as Meta


---- Html Infra ----

bodyTemplate :: String -> Html () -> Html ()
bodyTemplate title bod = doctypehtml_ $ do
    head_ $ do
        meta_ [charset_ "utf-8"]
        title_ (toHtml title)
        link_
            [ href_ "/css/style.css"
            , rel_ "stylesheet"
            , type_ "text/css" ]
    body_ $ do
        h1_ (toHtml title)
        bod


errorPage :: String -> Html ()
errorPage msg = bodyTemplate "Shiva" $
    div_ [class_ "warning"] $ do
        b_ "Error: "
        p_ $ toHtml msg

htmlFromEither :: (a -> Html ()) -> Either String a -> Html ()
htmlFromEither f (Right x)  = f x
htmlFromEither _ (Left str) = errorPage str


---- Main page ----

mainPage :: [ArticleMetadata] -> Html ()
mainPage xs = bodyTemplate "Shiva" $ do
    h2_ "Feed Sources"
    div_ [class_ "contentarea"] $ do
        h3_ "Dagens Nyheter"
        ul_ $ mapM_ sourceItem sources
    h2_ "Recently viewed"
    div_ [class_ "contentarea"] .
        ul_ . forM_ xs $ \meta ->
            li_ $ a_ [href_ $ "/content/dn/" <> Meta.urlFrag meta] (toHtml $ Meta.svTitle meta)



sourceItem :: Source -> Html ()
sourceItem i = li_ $
  a_ [href_ $ "sources/" <> titleCode i] . toHtml $ sourceTitle i


---- Feed listing pages ----

dnTitle :: String
dnTitle = "Dagens Nyheter"

feedPage :: FeedData -> Html ()
feedPage (FeedData xs errs) = bodyTemplate dnTitle $ do
    unless (null errs) .
        div_ [class_ "warning"] $ do
            p_ "Errors were encountered for feed items: "
            ul_ $ mapM_ (li_ . toHtml) errs
    mapM_ renderFeedItem xs


renderFeedItem :: FeedItem -> Html ()
renderFeedItem d = do
    div_ [class_ "feedmetadata"] $
        table_ [width_ "100%"] $ tr_ $ do
            td_ $ toHtml $ showTime (itemTime d)
            td_ [makeAttribute "align" "right"] $ do
                "Original content: "
                a_ [href_ $ urlFull d] "Link"
    div_ [class_ "svenska"] $
        a_ [href_ $ "/content/dn/" <> urlFrag d] (toHtml $ svTitle d)
    div_ [class_ "engelska"] (toHtml $ enTitle d)
    br_ []
    br_ []


---- Article content pages ----

articlePage :: TransArticle -> Html ()
articlePage TransArticle {..} = bodyTemplate "Shiva" $ do
    h2_ $ toHtml thetitle
    p_ $ do
        "Translated from "
        a_ [href_ origUrl] "the original."
    forM_ imageUrl $ \url -> do
        img_ [src_ url, class_ "articleImg"]
        br_ []
        br_ []
    mapM_ renderPair bodyResult

renderPair :: Sentence -> Html ()
renderPair s = do
    div_ [class_ "svenska"]  (toHtml $ fromText s)
    div_ [class_ "engelska"] (toHtml $ toText s)
    br_ []
    br_ []
