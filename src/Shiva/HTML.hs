{-# LANGUAGE OverloadedStrings,
             RecordWildCards #-}

module Shiva.HTML (

  -- * Pages
  feedPage,
  mainPage,
  articlePage,
  errorPage,

  -- * Helpers
  htmlFromEither

) where

import Shiva.Translation
import Shiva.Config      (Source (..), titleCode)
import Shiva.Sources     (sources)
import Shiva.Feeds

import Lucid.Base
import Lucid.Html5
import Data.Monoid ((<>))
import Data.Maybe (isJust,fromJust)
import Control.Monad (when)


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
errorPage msg = bodyTemplate "Shiva Translate" $
  div_ [class_ "warning"] $ do
    b_ "Error: "
    p_ $ toHtml msg

htmlFromEither :: (a -> Html ()) -> Either String a -> Html ()
htmlFromEither f (Right x)  = f x
htmlFromEither _ (Left str) = errorPage str


---- Main page ----

mainPage :: Html ()
mainPage = bodyTemplate "Shiva Translation" $ do
  h2_ "Feed Sources"
  div_ [class_ "contentarea"] $ do
    h3_ "Dagens Nyheter"
    ul_ $ mapM_ sourceItem sources
  h2_ "Swedish Reddit"
  div_ [class_ "contentarea"] "Coming soon..."

sourceItem :: Source -> Html ()
sourceItem i = li_ $
  a_ [href_ $ "sources/" <> titleCode i] . toHtml $ sourceTitle i


---- Feed listing pages ----

dnTitle :: String
dnTitle = "Articles from Dagens Nyheter"

feedPage :: FeedData -> Html ()
feedPage (FeedData xs errs)
  | null errs = bodyTemplate dnTitle . mapM_ renderFeedItem $ xs
  | otherwise = bodyTemplate dnTitle $ do
    div_ [class_ "warning"] $ do
      p_ "Errors were encountered for feed items: "
      ul_ $ mapM_ (li_ . toHtml) errs
    mapM_ renderFeedItem xs


renderFeedItem :: FeedItem -> Html ()
renderFeedItem d = do
  div_ [class_ "feedmetadata"] $
    table_ [width_ "100%"]$ tr_ $ do
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
articlePage TransArticle {..} = bodyTemplate "Shiva Translate" $ do
  h2_ $ toHtml thetitle
  p_ $ do
    "Translated from "
    a_ [href_ origUrl] "the original."
  when (isJust imageUrl) $ do
    img_ [src_ $ fromJust imageUrl, class_ "articleImg"]
    br_ []
    br_ []
  mapM_ renderPair (result bodyResult)

renderPair :: SvenskaPair -> Html ()
renderPair sp = do
  div_ [class_ "svenska"]  (toHtml $ swedish sp)
  div_ [class_ "engelska"] (toHtml $ english sp)
  br_ []
  br_ []
