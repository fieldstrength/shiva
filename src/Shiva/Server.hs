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

runHtmlGen :: ShivaConfig -> (a -> Html ()) -> ShivaM a -> IO (Html ())
runHtmlGen conf f =
    fmap (either error id) -- not unsafe due to 'catchErrorPage' below
  . runExceptT
  . flip runReaderT conf
  . catchErrorPage
  . fmap f

generateFeedPage :: ShivaConfig -> String -> IO (Html ())
generateFeedPage conf t = runHtmlGen conf feedPage (loadFeedDataByTitle t)

generateContentPage :: ShivaConfig -> String -> IO (Html ())
generateContentPage conf t = runHtmlGen conf resultPage (generateResultFromName t)


---- Server ----

getStaticPath :: IO FilePath
getStaticPath = getDataFileName "static"

server :: FilePath -> ShivaConfig -> IO ()
server staticPath conf = scotty 7777 $ do
  middleware $ staticPolicy (noDots >-> addBase staticPath)

  get "/" $ html . renderText $ mainPage

  get "/sources/:x" $ do
    x <- param "x"
    html =<< renderText <$> liftIO (generateFeedPage conf x)

  get "/content/dn/:x" $ do
    x <- param "x"
    html =<< renderText <$> liftIO (generateContentPage conf x)


-- | Run the server (as an installed executable).
runServer :: IO ()
runServer = do
  path <- getStaticPath
  mconf <- runExceptT loadConfig
  case mconf of
    Right conf -> server path conf
    Left err   -> do
      putStrLn "No valid config file found. Try running 'shiva setup' first."
      putStrLn $ "Details: " ++ err

-- | Run the server (from within ghci). Uses the repo's 'static' path instead of one
--   set up by Stack or Cabal.
test :: IO ()
test = runIOX loadConfig >>= server "static"



{-
get :: RoutePattern -> ActionM () -> ScottyM ()
param :: Parsable a => Text -> ActionM a
params :: ActionM [Param]
html :: Text -> ActionM ()
renderText :: Html a -> Text    -}
