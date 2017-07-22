{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- | The configuration data type and IO operations to set up, save and load it.
--   Also aliased for the different layers of the monad transformer stack we employ.
module Shiva.Config (

  -- * Configuration and App data
  ShivaConfig (..),
  Source (..),
  titleCode,
  ShivaData (..),

  -- ** Load data
  loadEverything,

  -- ** Setup
  setup,
  setupDB,

  -- * Monads
  IOX,
  ShivaM (..),
  CounterM,

  runIOX,

  -- * Exceptions
  ShivaException (..),

  -- ** Reader environment access
  appConfig,
  appConnection,
  appSources,
  srcLookup,
  codeLookup

) where

import Paths_shiva                (getDataFileName)

import Control.Applicative        ((<|>))
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent.STM.TVar
import Data.Bifunctor             (first)
import Data.ByteString            (writeFile)
import Data.Char                  (toLower)
import Data.List                  (isSuffixOf)
import Data.Maybe                 (fromMaybe)
import Data.MonoTraversable       (omap)
import Data.Text                  (Text, intercalate, words, pack)
import Data.Text.Encoding.Error   (UnicodeException)
import Data.Yaml.Aeson            (FromJSON, ToJSON, decodeFileEither, encode)
import Database.PostgreSQL.Simple
import GHC.Generics               (Generic)
import Translator
import Network.HTTP.Conduit       (HttpException)
import Prelude                    hiding (lookup, words, writeFile)
import Safe                       (headMay)
import System.Environment         (lookupEnv)
import System.Process             (callCommand)


-- | Application data saved in a local config file. Information for Microsoft Translator API and
--   database name and user.
data ShivaConfig = Config
  { translatorSubKey :: SubscriptionKey
  , dbName           :: String
  , dbUser           :: String
  } deriving (Show, Generic)

instance FromJSON ShivaConfig
instance ToJSON ShivaConfig

connectInfo :: ShivaConfig -> ConnectInfo
connectInfo Config {..} = defaultConnectInfo
    { connectUser = dbUser
    , connectDatabase = dbName
    }


-- | Data related to a source of news articles accessed via RSS.
data Source = Source
  { sourceTitle      :: Text
  , feedUrl          :: String
  , contentExtractor :: Text -> Text
  , imageExtractor   :: Text -> Maybe Text }


-- | The representation of the source title used in the corresponding URL (lowercase, dash-separated).
titleCode :: Source -> Text
titleCode = intercalate "-" . words . omap toLower . sourceTitle

-- | All data needed
data ShivaData = ShivaData
    { config      :: ShivaConfig
    , connection  :: Connection
    , sourceList  :: [Source]
    , transDataTV :: TVar TransData
    }


-----

type IOX = ExceptT String IO

newtype ShivaM a
    = ShivaM { runShivaM :: ReaderT ShivaData IO a }
    deriving (Functor, Applicative, Monad, MonadReader ShivaData, MonadIO, MonadThrow, MonadCatch)

type CounterM = StateT Int ShivaM


runIOX :: IOX a -> IO a
runIOX (ExceptT io) = do
  mx <- io
  case mx of
    Left str -> error str
    Right x  -> pure x


-----

data ShivaException
    = MSTranslatorException TranslatorException
    | PraseRSSFeedException String
    | NetworkException HttpException
    | TextException UnicodeException
    | UnknownFeed Text
    | UnknownSourceName Text
    | MissingArticle Text
    | Wronglengths
    deriving Show

instance Exception ShivaException


-----

appConfig :: ShivaM ShivaConfig
appConfig = asks config

appConnection :: ShivaM Connection
appConnection = asks connection

appSources :: ShivaM [Source]
appSources = asks sourceList

-- | Look up a source by name.
srcLookup :: Text -> ShivaM (Maybe Source)
srcLookup name = do
  srcs <- appSources
  pure $ headMay [ x | x <- srcs, sourceTitle x == name ]

-- | Look up a source by title code, the name format used in URLs.
codeLookup :: Text -> ShivaM (Maybe Source)
codeLookup code = do
  srcs <- appSources
  pure $ headMay [ x | x <- srcs, titleCode x == code ]


-----

homePath :: IOX FilePath
homePath = ExceptT $ do
  mh <- lookupEnv "HOME"
  case mh of
    Nothing   -> pure $ Left "No HOME environment variable."
    Just home -> pure $ Right $ slashPad home

slashPad :: String -> String
slashPad str = if "/" `isSuffixOf` str then str else str ++ "/"

configHomePath :: IOX FilePath
configHomePath = (++ ".shiva.yaml") <$> homePath

configPath :: IO FilePath
configPath = getDataFileName "config.yaml"

loadHomeConfig :: IOX ShivaConfig
loadHomeConfig = configHomePath >>= ExceptT . fmap (first show) . decodeFileEither

loadPathsConfig :: IOX ShivaConfig
loadPathsConfig = liftIO configPath >>= ExceptT . fmap (first show) . decodeFileEither

loadConfig :: IOX ShivaConfig
loadConfig = loadHomeConfig <|> loadPathsConfig

-- | Load all data needed by the application and initiate database connection.
loadEverything :: [Source] -> IOX ShivaData
loadEverything srcs = do
  conf <- loadConfig
  conn <- liftIO $ connect (connectInfo conf)
  tdata <- ExceptT $ first show <$> initTransDataIO (translatorSubKey conf)
  tvar <- liftIO $ newTVarIO tdata
  pure $ ShivaData conf conn srcs tvar


----- Setup -----

enterConfig :: IO ShivaConfig
enterConfig = do
  muser <- lookupEnv "USER"
  let usr = fromMaybe "" muser
  putStrLn "Enter MS Translator subscription key:"
  subKey <- pack <$> getLine
  putStrLn "Enter database name (blank = 'shiva'): "
  dbn <- getLine
  let dbname = if null dbn then "shiva" else dbn
  if null usr then putStrLn "Enter database user name: "
              else putStrLn $ "Enter database user name (blank = '" ++ usr ++ "')"
  dbuser <- getLine
  pure $ Config subKey dbname dbuser

yesOrNo :: IO Bool
yesOrNo = do
  s <- getLine
  case s of
    'y':_ -> pure True
    'n':_ -> pure False
    _     -> putStrLn "Please answer yes or no (y/n): " *> yesOrNo

-- | Command line script to enter and save app configuration data.
setup :: IO ()
setup = do
  conf <- enterConfig
  mh <- fmap slashPad <$> lookupEnv "HOME"
  case mh of
    Nothing -> configPath >>= flip writeFile (encode conf)
    Just h  -> do
      putStrLn "Save (hidden) config file in home directory?"
      putStrLn "This will make it stable across package reinstalls. Enter (y/n): "
      b <- yesOrNo
      path <- if b then pure (h ++ ".shiva.yaml")
                   else configPath
      writeFile path (encode conf)
      putStrLn $ "Configuration saved at " ++ path ++ "."
      putStrLn "Create and set up the database?"
      db <- yesOrNo
      if db then setupDB else pure ()
      putStrLn "Setup complete!"

setupDB :: IO ()
setupDB = do
  Config {..} <- runIOX loadConfig
  sqlFile <- getDataFileName "database/setup.sql"
  callCommand $ "createdb " ++ dbName ++ " --owner=" ++ dbUser
  callCommand $ "psql " ++ dbName ++ " -f " ++ sqlFile
