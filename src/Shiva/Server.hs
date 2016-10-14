{-# LANGUAGE OverloadedStrings #-}

module Shiva.Server (
  runServer,
  test
) where

import Shiva.HTML
import Shiva.Execute
import Shiva.Config
import Paths_shiva (getDataFileName)

import Web.Scotty
import Lucid
import Network.Wai.Middleware.Static
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)


---- Page generation ----

runHtmlGen :: ShivaData -> (a -> Html ()) -> ShivaM a -> IO (Html ())
runHtmlGen d f =
    fmap (either error id) -- not unsafe due to 'catchErrorPage' below
  . runExceptT
  . flip runReaderT d
  . catchErrorPage
  . fmap f


generateFeedPage :: ShivaData -> String -> IO (Html ())
generateFeedPage d t = runHtmlGen d feedPage (loadFeedDataByTitle t)

generateContentPage :: ShivaData -> String -> IO (Html ())
generateContentPage d t = runHtmlGen d resultPage (generateResultFromName t)


---- Server ----

getStaticPath :: IO FilePath
getStaticPath = getDataFileName "static"

server :: FilePath -> ShivaData -> IO ()
server staticPath d = scotty 7777 $ do
  middleware $ staticPolicy (noDots >-> addBase staticPath)

  get "/" $ html . renderText $ mainPage

  get "/sources/:x" $ do
    x <- param "x"
    html =<< renderText <$> liftIO (generateFeedPage d x)

  get "/content/dn/:x" $ do
    x <- param "x"
    html =<< renderText <$> liftIO (generateContentPage d x)


-- | Run the server (as an installed executable).
runServer :: IO ()
runServer = do
  path <- getStaticPath
  mconf <- runExceptT loadEverything
  case mconf of
    Right conf -> server path conf
    Left err   -> do
      putStrLn "No valid config file found. Try running 'shiva setup' first."
      putStrLn $ "Details: " ++ err

-- | Run the server (from within ghci). Uses the repo's 'static' path instead of one
--   set up by Stack or Cabal.
test :: IO ()
test = runIOX loadEverything >>= server "static"



{-
get :: RoutePattern -> ActionM () -> ScottyM ()
param :: Parsable a => Text -> ActionM a
params :: ActionM [Param]
html :: Text -> ActionM ()
renderText :: Html a -> Text    -}
