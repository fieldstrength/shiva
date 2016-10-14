{-# LANGUAGE DeriveGeneric,
             OverloadedStrings,
             RecordWildCards #-}

-- | The configuration data type and IO operations to set up, save and load it.
--   Also aliased for the different layers of the monad transformer stack we employ.
module Shiva.Config (

  -- * Configuration and App data
  ShivaConfig (..),
  ShivaData (..),

  -- ** Load data

  loadConfig,
  loadEverything,

  -- ** Setup
  setup,


  -- * Monads
  IOX,
  ShivaM,
  CounterM,

  runIOX,

-- ** Reader environment access

  appConfig,
  appConnection,

) where

import Paths_shiva          (getDataFileName)

import GHC.Generics         (Generic)
import Data.Yaml.Aeson      (FromJSON, ToJSON, encode, decodeFileEither)
import Data.List            (isSuffixOf)
import System.Environment   (lookupEnv)
import Data.ByteString      (writeFile)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor       (first)
import Data.Maybe           (fromMaybe)
import Control.Applicative  ((<|>))
import Prelude hiding       (writeFile)
import Database.PostgreSQL.Simple



data ShivaConfig = Config
  { clientId :: String
  , clientSecret :: String
  , dbName :: String
  , dbUser :: String
  } deriving (Show, Generic)

instance FromJSON ShivaConfig
instance ToJSON ShivaConfig

connectInfo :: ShivaConfig -> ConnectInfo
connectInfo Config {..} = defaultConnectInfo { connectUser = dbUser, connectDatabase = dbName }


data ShivaData = ShivaData
  { config :: ShivaConfig
  , connection :: Connection }


-----


type IOX = ExceptT String IO

type ShivaM = ReaderT ShivaData IOX

type CounterM = StateT Int ShivaM



runIOX :: IOX a -> IO a
runIOX (ExceptT io) = do
  mx <- io
  case mx of
    Left str -> error str
    Right x  -> return x


-----


appConfig :: ShivaM ShivaConfig
appConfig = asks config

appConnection :: ShivaM Connection
appConnection = asks connection


-----

homePath :: IOX FilePath
homePath = ExceptT $ do
  mh <- lookupEnv "HOME"
  case mh of
    Nothing   -> return $ Left "No HOME environment variable."
    Just home -> return $ Right $ slashPad home

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

loadEverything :: IOX ShivaData
loadEverything = do
  conf <- loadConfig
  conn <- liftIO $ connect (connectInfo conf)
  return $ ShivaData conf conn


----- Setup -----


enterConfig :: IO ShivaConfig
enterConfig = do
  muser <- lookupEnv "USER"
  let usr = fromMaybe "" muser
  putStrLn "Enter MS Translator Client ID:"
  cid <- getLine
  putStrLn "Enter MS Translator Client Secret: "
  csecret <- getLine
  putStrLn "Enter database name (blank = 'shivadb'): "
  dbn <- getLine
  let dbname = if null dbn then "shivadb" else dbn
  if null usr then putStrLn "Enter database user name: "
              else putStrLn $ "Enter database user name (blank = '" ++ usr ++ "')"
  dbuser <- getLine
  let conf = Config cid csecret dbname dbuser
  return conf

yn :: IO Bool
yn = do
  s <- getLine
  case s of
    'y':_ -> return True
    'n':_ -> return False
    _     -> putStrLn "Please answer yes or no (y/n): " *> yn

setup :: IO ()
setup = do
  conf <- enterConfig
  mh <- fmap slashPad <$> lookupEnv "HOME"
  case mh of
    Nothing -> configPath >>= flip writeFile (encode conf)
    Just h  -> do
      putStrLn "Save (hidden) config file in home directory?"
      putStrLn "This will make it stable across package reinstalls. Enter (y/n): "
      b <- yn
      path <- if b then return (h ++ ".shiva.yaml")
                   else configPath
      writeFile path (encode conf)
      putStrLn $ "Configuration saved at " ++ path ++ "."
