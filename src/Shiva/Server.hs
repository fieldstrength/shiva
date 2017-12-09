{-# LANGUAGE OverloadedStrings #-}

module Shiva.Server (
  runServer,
  testServer
) where

import Paths_shiva                   (getDataFileName)
import Shiva.Config
import Shiva.Execute
import Shiva.HTML
import Shiva.Sources

import Control.Monad.Except          (runExceptT)
import Control.Monad.IO.Class        (liftIO)
import Control.Monad.Reader          (runReaderT)
import Data.Text                     (Text)
import Lucid
import Network.Wai.Middleware.Static
import Shiva.Table.ArticleContent   (getRecent, insert)
import Web.Scotty


---- Page generation ----

runHtmlGen :: ShivaData -> (a -> Html ()) -> ShivaM a -> IO (Html ())
runHtmlGen d f = flip runReaderT d
               . runShivaM
               . catchErrorPage
               . fmap f


generateFeedPage :: ShivaData -> Text -> IO (Html ())
generateFeedPage d t = runHtmlGen d feedPage (loadFeedByTitleCode t)

generateContentPage :: ShivaData -> Text -> IO (Html ())
generateContentPage d t = runHtmlGen d articlePage $ do
    art <- generateResultFromName t
    art <$ insert [art]

generateMainPage :: ShivaData -> IO (Html ())
generateMainPage d = runHtmlGen d mainPage (getRecent 20)

---- Server ----

getStaticPath :: IO FilePath
getStaticPath = getDataFileName "static"

server :: FilePath -> ShivaData -> IO ()
server staticPath d = scotty 7777 $ do
    middleware $ staticPolicy (noDots >-> addBase staticPath)

    get "/" $
        html =<< renderText <$> liftIO (generateMainPage d)

    get "/sources/:x" $ do
        x <- param "x"
        html =<< renderText <$> liftIO (generateFeedPage d x)

    get "/content/dn/:x" $ do
        x <- param "x"
        html =<< renderText <$> liftIO (generateContentPage d x)


-- | Run the server (as an installed executable).
runServer :: [Source] -> IO ()
runServer srcs = do
    path <- getStaticPath
    mconf <- runExceptT (loadEverything srcs)
    case mconf of
        Right conf -> server path conf
        Left err   -> do
            putStrLn "No valid config file found. Try running 'shiva setup' first."
            putStrLn $ "Details: " ++ err

-- | Run the server (from within ghci). Uses the repo's 'static' path instead of one
--   set up by Stack or Cabal. Useful for testing out changes in styling or other static aspects.
testServer :: IO ()
testServer = runIOX (loadEverything sources) >>= server "static"



{-
get :: RoutePattern -> ActionM () -> ScottyM ()
param :: Parsable a => Text -> ActionM a
params :: ActionM [Param]
html :: Text -> ActionM ()
renderText :: Html a -> Text    -}
